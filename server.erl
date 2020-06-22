-module(server).
-include("header.hrl").
-import(lists, [nth/2, member/2, append/2, delete/2, map/2]).
-import(tateti, [make_play/3]).
-export([start/0, dispatcher/1, psocket/3, pcomando/4, pupdater/1, master/3]).
-export([lsgRequest/2]).


% Inicializamos el server.
start() ->
  GamesMap = maps:new(),
  MasterPid = spawn(?MODULE, master, [[], GamesMap, 0]),
  register(master, MasterPid),
  {ok, LSocket} = gen_tcp:listen(?PORT,
                                 [binary,
                                  {packet, 0},
                                  {active, false}]),
  dispatcher(LSocket).

lsgRequest([], _)-> [];
lsgRequest([Hd | Tl], CMDId)->
  {master, Hd} ! {lsg, CMDId, self()},
  receive
    {ok, CMDId, Lsg} -> Lsg ++ lsgRequest(Tl, CMDId)
  end.

% Master loop que se encarga de mantener actualizada la info de las partidas
% y los jugadores.
master(Players, Games, Count) -> 
  receive
    % Ingresa nuevo usuario.
    {con, Name, PID} -> 
      case member(Name, Players) of
        true -> PID ! {error, "Name taken"},
                master(Players, Games, Count);
        false -> PID ! ok,
                 master(append([Name], Players), Games, Count)
      end;

% GameIDs = keys(Games)
% LSG = map(fun -> {key, key#game.p1,.....}, Games)

    % Se pide una lista de todos los juegos disponibles.
    {lsg, CMDId, PID} -> 
      IDs = maps:keys(Games),
      LSG = map(fun(Game) -> {Game, Game#game.p1, Game#game.p2, Game#game.obs} end, IDs),
      PID ! {ok, CMDId, LSG},
      master(Players, Games, Count);

    % Crea un nuevo juego.
    {new, CMDId, Name, Updater, PID} -> 
      GameId = {Count, node()},
      Game = #game{p1 = Name, obs = [], moves = 0, updaters = [Updater]}, 
      NGames = maps:put(GameId, Game),
      PID ! {ok, CMDId, GameId},
      master(Players, NGames, Count + 1);

    % Acepta un juego.
    {acc, CMDId, GameId, Name, Updater, PID} -> 
      case maps:find(GameId, Games) of
        error -> PID ! {error, CMDId, "Game not found"},
                 master(Players, Games, Count);
        {ok, Game} ->
          if 
            Game#game.p2 /= undefined -> 
              PID ! {error, CMDId, "Game already on course"},
              master(Players, Game, Count);
            true -> 
              NGame = Game#game{p2 = Name, board = ?CLEANBOARD, updaters = append(Updater, Game#game.updaters)},
              NGames = maps:update(GameId, NGame, Games),
              PID ! {ok, CMDId},
              master(Players, NGames, Count)
          end
      end;

    % Realizar una jugada.
    {pla, CMDId, GameId, Play, Name, PID} -> 
      case maps:find(GameId, Games) of
        error -> 
          PID ! {error, CMDId, "Game not found"},                 
          master(Players, Games, Count);
        {ok, Game} -> 
          case make_play(Game, Play, Name) of
            {error, Reason} ->
              PID ! {error, CMDId, Reason},
              master(Players, Games, Count);
            {Result, NGame} -> 
              NGames = maps:update(GameId, NGame, Games),
              PID ! {ok, CMDId, NGame, Result},
              master(Players, NGames, Count);
            NGame ->
              NGames = maps:update(GameId, NGame, Games),
              PID ! {ok, CMDId, NGame},
              master(Players, NGames, Count)
          end
      end;

    % Observar una partida.
    {obs, CMDId, GameId, Name, PID} ->
      case maps:find(GameId, Games) of
        error -> 
          PID ! {error, CMDId, "Game not found"},                 
          master(Players, Games, Count);
        {ok, Game} -> 
          case member(Name, Game#game.obs) of
            true -> 
              PID ! {error, CMDId, "Already observing"},
              master(Players, Games, Count);
            false ->
              NGame = Game#game{obs = Game#game.obs ++ Name},
              NGames = maps:update(GameId, NGame, Games),
              PID ! {ok, CMDId},          
              master(Players, NGames, Count)
          end
      end;

    % Dejar de observar una partida.
    {lea, CMDId, GameId, Name, PID} -> 
      case maps:find(GameId, Games) of
        error ->
          PID ! {error, CMDId, "Game not found"},
          master(Players, Games, Count);
        {ok, Game} -> 
          case member(Name, Game#game.obs) of
            false -> 
              PID ! {error, CMDId},
              master(Players, Games, Count);
            true ->
              NGame = Game#game{obs = delete(Name, Game#game.obs)},
              NGames = maps:update(GameId, NGame, Games),
              PID ! {ok, CMDId}, 
              master(Players, NGames, Count)
          end
      end;
    % Abandona todos los juegos en los que participe.
    {bye, Name, PID} -> ok;

    _ -> ok
  end.

% Acepta las conexiones entrantes y crea un hilo que se encargará 
% de los pedidios de ese cliente. --> (hacer que mande al cliente al nodo con menos carga)
dispatcher(LSocket) -> 
  {ok, Socket} = gen_tcp:accept(LSocket),
  Updater = spawn(?MODULE, pupdater, [Socket]),
  spawn(?MODULE, psocket, [Socket, undefined, Updater]),
  dispatcher(LSocket).

% Hilo que atiende los pedidos de un cliente determinado.
psocket(Socket, undefined, Updater) ->
  case gen_tcp:recv(Socket,0 ) of
    {ok, Packet} ->
      Request = binary_to_term(Packet),
      case Request of
        {con, Name} -> 
          spawn(?MODULE, pcomando, [{con, Name}, undefined, Updater, Socket]),
          psocket(Socket, Name, Updater);
        _ -> gen_tcp:send(Socket, {error, "Name undefined"})
      end;
    {error, close} -> 
      Updater ! bye,
      ok
  end;
psocket(Socket, Name, Updater) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Packet} ->
      Request = binary_to_term(Packet),
      spawn(?MODULE, pcomando, [Request, Name, Updater, Socket]),
      psocket(Socket, Name, Updater);
    {error, close} ->
      spawn(?MODULE, pcomando, [{bye, Name}]),
      updater ! bye,
      io:format("~p disconnected ~n", [Name])
  end.

% Hilo de comunicación del servidor al cliente para actualizar estados
% no para respuestas a pedidos del cliente.
pupdater(Socket) ->
  receive 
    bye -> ok;
    Update -> gen_tcp:send(Socket, Update),
              pupdater(Socket)
  end.

% Agrega un nuevo usuario al servidor.
pcomando({con, NewName}, Name, _, Socket)->
  if 
    Name /= undefined -> gen_tcp:send(Socket, {error, "Name already defined"});
    true -> 
      master ! {con, NewName, self()},
      receive
        Response -> gen_tcp:send(Socket, Response)
      end;
% Lista los juegos disponibles.
pcomando({lsg, CMDId}, _, _, Socket) ->
  Response = {ok, CMDId, lsgRequest(append(node(),nodes()), CMDId)},
  gen_tcp:send(Socket, Response);

% Inicia nuevo juego.
pcomando({new, CMDId}, Name, Updater, Socket) -> 
  master ! {new, CMDId, Name, Updater, self()},
  receive
    Response -> gen_tcp:send(Socket, Response)
  end;
% Acepta un juego.
pcomando({acc, CMDId, GameId}, Name, Updater, Socket) -> 
  master ! {acc, CMDId, GameId, Name, Updater, self()},
  receive
    Response -> Response
  end;
% Hacer una jugada.
pcomando({pla, CMDId, GameId, Play, Name}) -> 
   master ! {pla, CMDId, GameId, Name, Play, self()},
  receive
    {ok, Game, Obs} -> Response
  end;
% Observar un juego
pcomando({obs, CMDId, GameId, Name}) -> 
  master ! {obs, CMDId, GameId, Name, self()},
  receive
    Response -> Response
  end;
pcomando({lea, CMDId, GameId, Name}) -> 
  master ! {lea, CMDId, GameId, Name, self()},
  receive
    Response -> Response
  end;
pcomando({bye, Name}) ->
  master ! {bye, Name, self()},
  receive
    Response -> Response
  end.