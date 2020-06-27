-module(client).
-include("header.hrl").
-export([start/2, prompt/0, input/1, sender/3, receiver/1, list_games/1, show_game/1, print_board/1]).

start(IP, Port)->
    case gen_tcp:connect(IP, Port, [binary, {packet, 0}, {active,false}]) of
      {error, Reason} -> 
        io:format("Error en la conexión, motivo: ~p ~n", [Reason]);
      {ok, Socket} -> 
        io:format("Conexión exitosa ~n"),
        sender(Socket, undefined, init)
    end.

sender(Socket, undefined, init) ->
  io:format("Antes de jugar debes definir tu nombre ~n"),
  Name = string:chomp(io:get_line("Ingrese su nombre: ")),
  case gen_tcp:send(Socket, term_to_binary({con, Name})) of
    {error, Reason} -> 
      io:format("Error al registrar su nombre, motivo: ~p ~n", [Reason]),
      sender(Socket, undefined, init);                       
    ok -> 
      case gen_tcp:recv(Socket, 0) of
        {error, Reason} -> 
          io:format("Error al registrar su nombre, motivo: ~p ~n", [Reason]),
          sender(Socket, undefined, init);
        {ok, Packet} ->
          Response = binary_to_term(Packet),
          case Response of 
            {error, Reason} -> 
              io:format("Error al registrar su nombre, motivo: ~p ~n", [Reason]),
              sender(Socket, undefined, init);
            ok -> 
              io:format("Bienvenido al servidor ~p ~n", [Name]),
              spawn(?MODULE, receiver, [Socket]),
              sender(Socket, Name, 0)
          end
      end
  end;
sender(Socket, Name, Count) ->
  io:format("----------------------------- ~n"),
  io:format("----------------------------- ~n"),
  prompt(),
  case input(Count) of
    bye -> 
      io:format("----------------------------- ~n"),
      io:format("----------------------------- ~n"),
      gen_tcp:send(Socket, term_to_binary(bye)),
      gen_tcp:close(Socket);
    Request -> 
      io:format("----------------------------- ~n"),
      io:format("----------------------------- ~n"),
      gen_tcp:send(Socket, term_to_binary(Request)),
      timer:sleep(1000),
      sender(Socket, Name, Count + 1)
  end.

prompt() -> 
  io:format("Sus opciones son: ~n"),
  io:format("1) Listar los juegos disponibles. ~n"),
  io:format("2) Crear un nuevo juego. ~n"),
  io:format("3) Aceptar un juego disponible. ~n"),
  io:format("4) Realizar una jugada en un juego en el que participes. ~n"),
  io:format("5) Abandonar un juego en el que participes. ~n"),
  io:format("6) Observar un juego. ~n"),
  io:format("7) Dejar de observar un juego. ~n"),
  io:format("99) Terminar la conexión, abandonando todos los juegos en los que participes. ~n").

input(CMDId) -> 
  case io:fread("Ingrese una opción: ", "~d") of
    {ok, [Option]} ->
      io:format("Ingrese los datos que se le pidan separados por un espacio ~n"),
      case Option of
        1 -> 
          {lsg, CMDId};
        2 -> 
          {new, CMDId};
        3 -> 
          case io:fread("Ingrese el número del juego y el nombre del nodo donde se encuentra: ", "~d ~s") of 
            {ok, [N, Node]} -> 
              {acc, CMDId, {N, list_to_atom(Node)}};
            _ -> 
              io:format("Entrada inválida ~n"),
              input(CMDId)
            end;
        4 ->
          case io:fread("Ingrese el número del juego, el nombre del nodo donde se encuentra, una fila y una columna del tablero ", "~d ~s ~d ~d") of
            {ok, [N, Node, Row, Column]} ->
              {pla, CMDId, {N, list_to_atom(Node)}, {Row, Column}};
            _ -> io:format("Entrada inválida ~n"),
                 input(CMDId)
          end;
        5 ->
          case io:fread("Ingrese el número del juego y el nombre del nodo donde se encuentra: ", "~d ~s") of 
            {ok, [N, Node]} -> 
              {pla, CMDId, {N, list_to_atom(Node)}, leave};
            _ -> 
              io:format("Entrada inválida ~n"),
              input(CMDId)
            end;
        6 ->
          case io:fread("Ingrese el número del juego y el nombre del nodo donde se encuentra: ", "~d ~s") of 
            {ok, [N, Node]} -> 
              {obs, CMDId, {N, list_to_atom(Node)}};
            _ -> 
              io:format("Entrada inválida ~n"),
              input(CMDId)
            end;
        7 -> 
          case io:fread("Ingrese el número del juego y el nombre del nodo donde se encuentra: ", "~d ~s") of 
            {ok, [N, Node]} -> 
              {lea, CMDId, {N, list_to_atom(Node)}};
            _ -> 
              io:format("Entrada inválida ~n"),
              input(CMDId)
            end;
        99 -> 
          bye;
        _ -> io:format("Opción inválida ~n"),
             input(CMDId)
      end;
    {error, _} -> io:format("Entrada inválida ~n"),
                  input(CMDId)
  end.

list_games([]) -> ok;
list_games([{{N, Node}, P1, P2, Obs} | Tl]) ->
  io:format("Juego n° ~p en el nodo ~p: ", [N, Node]),
  case P2 of 
    undefined -> io:format("~p está buscando rival, observan: ~p ~n", [P1, Obs]),
                 list_games(Tl);
    _ -> io:format("~p contra ~p, observan ~p ~n", [P1, P2, Obs]),
         list_games(Tl)
  end.

print_cell(Cell) ->
  case Cell of
    o -> 'O';
    x -> 'X';
    empty -> '-'
  end.

print_board({A11, A12, A13, A21, A22, A23, A31, A32, A33}) ->
  io:format("~p|~p|~p ~n", [print_cell(A11), print_cell(A12), print_cell(A13)]),
  io:format("------------ ~n"),
  io:format("~p|~p|~p ~n", [print_cell(A21), print_cell(A22), print_cell(A23)]),
  io:format("------------ ~n"),
  io:format("~p|~p|~p ~n", [print_cell(A31), print_cell(A32), print_cell(A33)]).

show_game(Game)->
  io:format("Juegan ~p contra ~p, es el turno de ~p y se realizaron ~p movimientos ~n", [Game#game.p1, Game#game.p2, Game#game.turn, Game#game.moves]),
  print_board(Game#game.board).
  
receiver(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Packet} -> 
      case binary_to_term(Packet) of
        {ok, CMDId} -> 
           io:format("Comando ~p realizado exitosamente ~n", [CMDId]),
           receiver(Socket);
        {error, CMDId, Reason} -> 
          io:format("Error al realizar el comando ~p, motivo: ~p ~n", [CMDId, Reason]),
          receiver(Socket);
        {ok, lsg, CMDId, Lsg} ->
          io:format("Se realizó el comando ~p, mostrando lista de juegos ~n", [CMDId]),
          if
            Lsg == [] -> 
              io:format("No hay juegos en curso ~n"),
              receiver(Socket);
            true ->
              list_games(Lsg),
              receiver(Socket)
            end;
        {ok, new, CMDId, {N, Node}} -> 
          io:format("Se realizó el comando ~p, iniciaste una nueva partida con el id: ~p en el nodo ~p ~n", [CMDId, N, Node]),
          receiver(Socket);
        {ok, obs, CMDId, {N, Node}, Game} ->
          io:format("Se realizó el comando ~p, estás observando la partida: ~p en el nodo ~p ~n", [CMDId, N, Node]),
          show_game(Game), 
          receiver(Socket);
        {upd, start, {N, Node}, Game} -> 
          io:format("Inicia la partida: ~p en el nodo ~p: ~n", [N, Node]),
          show_game(Game),
          receiver(Socket);
        {upd, pla, Name, {N, Node}, Game} -> 
          io:format("~p realizó un movimiento en la partida: ~p en el nodo ~p ~n", [Name, N, Node]),
          show_game(Game),
          receiver(Socket);
        {upd, pla, Name, {N, Node}, Game, Result} ->
          case Result of 
            tie ->
              io:format("La partida ~p en el nodo ~p terminó en empate ~n", [N, Node]),
              io:format("Tablero final ~n"),
              print_board(Game#game.board),
              receiver(Socket);
            win ->  
              io:format("~p ganó la partida: ~p en el nodo ~p ~n", [Name, N, Node]),
              io:format("Tablero final ~n"),
              print_board(Game#game.board),
              receiver(Socket)
          end;
        {upd, leave, Name, {N, Node}} ->
          io:format("~p abandonó la partida ~p en el nodo ~p ~n", [Name, N, Node]),
          receiver(Socket)
      end;
    {error, closed} -> io:format("Servidor cerrado ~n")
  end.