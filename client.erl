-module(client).
-include("header.hrl").
-export([start/2, prompt/0, input/1, sender/3, receiver/1]).

start(IP, Port)->
    case gen_tcp:connect(IP, Port, [binary, {packet, 0}, {active,false}]) of
      {error, Reason} -> 
        io:format("Error en la conexión, motivo: ~p ~n", [Reason]);
      {ok, Socket} -> 
        io:format("Conexión exitosa ~n"),
        sender(Socket, undefined, init)
    end.

receiver(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, A} -> io:format("~p ~n", [binary_to_term(A)]),
               receiver(Socket);
    {error, closed} -> io:format("Servidor cerrado ~n")
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
  prompt(),
  case input(Count) of
    bye -> gen_tcp:send(Socket, term_to_binary(bye)),
           gen_tcp:close(Socket);
    Request -> gen_tcp:send(Socket, term_to_binary(Request)),
               timer:sleep(1000),
               sender(Socket, Name, Count + 1)
  end.

prompt() -> 
  io:format("Sus opciones son: ~n"),
  io:format("1) Listar los juegos disponibles. ~n"),
  io:format("2) Crear un nuevo juego. ~n"),
  io:format("3) Aceptar un juego disponible. ~n"),
  io:format("4) Realizar una jugada en un juego en el que participes. ~n"),
  io:format("5) Observar un juego. ~n"),
  io:format("6) Dejar de observar un juego. ~n"),
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
            _ -> io:format("Entrada inválida ~n")
          end;
        5 ->
          case io:fread("Ingrese el número del juego y el nombre del nodo donde se encuentra: ", "~d ~s") of 
            {ok, [N, Node]} -> 
              {obs, CMDId, {N, list_to_atom(Node)}};
            _ -> 
              io:format("Entrada inválida ~n"),
              input(CMDId)
            end;
        6 -> 
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
    {error, _} -> io:format("Entrada inválida ~n")
  end.