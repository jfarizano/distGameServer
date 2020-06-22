-module(server).
-include("header.hrl").
-import(lists, [nth/2, member/2, append/2, delete/2]).
-import(tateti, [make_play/3]).
-export([start/0, dispatcher/1, master/3]).
-export([psocket/3, pcomando/4, pupdater/1]).
-export([lsgRequest/2, broadcast/2, delete_player/2]).


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

lsgRequest([], _) -> [];
lsgRequest([Hd | Tl], CMDId) ->
  {master, Hd} ! {lsg, CMDId, self()},
  receive
    {ok, CMDId, Lsg} -> Lsg ++ lsgRequest(Tl, CMDId)
  end.

broadcast([], _) -> ok;
broadcast([HdUpdater | Tl], Update) ->
  gen_tcp:send(HdUpdater, Update),
  broadcast(Tl, Update).

delete_player(Name, Games) ->
  ok.

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

    % Se pide una lista de todos los juegos disponibles.
    {lsg, CMDId, PID} -> 
      IDs = maps:keys(Games),
      LSG = lists:foldl(fun(ID, List) ->
                          Game = maps:get(ID, Games),
                          [{ID, Game#game.p1, Game#game.p2, Game#game.obs}] ++ List 
                          end, 
                        [], IDs),
      PID ! {ok, CMDId, LSG},
      master(Players, Games, Count);

    % Crea un nuevo juego.
    {new, CMDId, Name, Updater, PID} -> 
      GameId = {Count, node()},
      Game = #game{p1 = Name, obs = [], moves = 0, updaters = [Updater]}, 
      NGames = maps:put(GameId, Game, Games),
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
              PList = [Game#game.p1, Name], % Elegimos de quien es el primer turno aleatoriamente
              Index = rand:uniform(length(PList)),
              NGame = Game#game{p2 = Name, board = ?CLEANBOARD, turn = nth(Index, PList), updaters = append([Updater], Game#game.updaters)},
              NGames = maps:update(GameId, NGame, Games),
              PID ! {ok, CMDId, NGame, NGame#game.updaters},
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
          case tateti:make_play(Game, Play, Name) of
            {error, Reason} ->
              PID ! {error, CMDId, Reason},
              master(Players, Games, Count);
            {Result, NGame} -> 
              NGames = maps:update(GameId, NGame, Games),
              PID ! {ok, CMDId, NGame, Result, NGame#game.updaters},
              master(Players, NGames, Count);
            NGame ->
              NGames = maps:update(GameId, NGame, Games),
              PID ! {ok, CMDId, NGame, NGame#game.updaters},
              master(Players, NGames, Count)
          end
      end;

    % Observar una partida.
    {obs, CMDId, GameId, Name, Updater, PID} ->
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
              NGame = Game#game{obs = append([Name], Game#game.obs), updaters = append([Updater], Game#game.updaters)},
              NGames = maps:update(GameId, NGame, Games),
              PID ! {ok, CMDId},          
              master(Players, NGames, Count)
          end
      end;

    % Dejar de observar una partida.
    {lea, CMDId, GameId, Name, Updater, PID} -> 
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
              NGame = Game#game{obs = delete(Name, Game#game.obs), updaters = delete(Updater, Game#game.updaters)},
              NGames = maps:update(GameId, NGame, Games),
              PID ! {ok, CMDId}, 
              master(Players, NGames, Count)
          end
      end;
    % Abandona todos los juegos en los que participe.
    {bye, Name, Updater} ->
      % NGames = delete_player(Name, Games), 
      master(delete(Name, Players), Games, Count);
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
          spawn(?MODULE, pcomando, [{con, {Name, node()}}, undefined, Updater, Socket]),
          psocket(Socket, {Name, node()}, Updater);
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
      spawn(?MODULE, pcomando, [disconnect, Name, Updater, Socket]),
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
pcomando({con, NewName}, Name, _, Socket) ->
  if 
    Name /= undefined -> gen_tcp:send(Socket, {error, "Name already defined"});
    true -> 
      master ! {con, NewName, self()},
      receive
        Response -> gen_tcp:send(Socket, Response)
      end
  end;
% Lista los juegos disponibles.
pcomando({lsg, CMDId}, _, _, Socket) ->
  Response = {ok, CMDId, lsgRequest(append([node()], nodes()), CMDId)},
  gen_tcp:send(Socket, Response);
% Inicia nuevo juego.
pcomando({new, CMDId}, Name, Updater, Socket) -> 
  master ! {new, CMDId, Name, Updater, self()},
  receive
    Response -> gen_tcp:send(Socket, Response)
  end;
% Acepta un juego.
pcomando({acc, CMDId, {Count, Node}}, Name, Updater, Socket) -> 
  {master, Node} ! {acc, CMDId, {Count, Node}, Name, Updater, self()},
  receive
    {error, CMDId, Reason} -> gen_tcp:send(Socket, {error, CMDId, Reason});
    {ok, CMDId, Game, Updaters} -> broadcast(Updaters, {start, {Count, Node}, Game}),
                                   gen_tcp:send(Socket, {ok, CMDId})
  end;
% Hacer una jugada.
pcomando({pla, CMDId, {Count, Node}, Play}, Name, _, Socket) -> 
  {master, Node} ! {pla, CMDId, {Count, Node}, Name, Play, self()},
  receive
    {error, CMDId, Reason} -> gen_tcp:send(Socket, {error, CMDId, Reason});
    {ok, CMDId, Game, Result, Updaters} -> broadcast(Updaters, {pla, {Count, Node}, Game, Result});
    {ok, CMDId, Game, Updaters} -> broadcast(Updaters, {pla, {Count, Node}, Game})
  end;
% Observar un juego
pcomando({obs, CMDId, {Count, Node}}, Name, Updater, Socket) -> 
  {master, Node} ! {obs, CMDId, {Count, Node}, Name, Updater, self()},
  receive
    Response -> gen_tcp:send(Socket, Response)
  end;
pcomando({lea, CMDId, {Count, Node}}, Name, Updater, Socket) -> 
  {master, Node} ! {lea, CMDId, {Count, Node}, Name, Updater, self()},
  receive
    Response -> gen_tcp:send(Socket, Response)
  end;
pcomando(bye, Name, _, Socket) ->
  master ! {bye, Name, self()},
  receive
    Response -> gen_tcp:send(Socket, Response)
  end;
pcomando(disconnect, Name, _, _) ->
  master ! {bye, Name, self()},
  receive
    _ -> ok
  end.