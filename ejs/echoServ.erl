-module(echoServ).
-include("echoOpts.hrl").
-export([start/0,receptor/1,echoResp/1]).

start() ->
    {ok, LSocket} = gen_tcp:listen( ?Puerto
                                  , [ binary
                                    , {packet, 0}
                                    , {active, false}]),
    receptor(LSocket).

receptor(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(?MODULE, echoResp,[Socket]),
    receptor(LSocket).

echoResp(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Paquete} ->
            {cmd, CMDid, Args} = binary_to_term(Paquete),
            io:format("Me llegó: un átomo ~p, un id ~p, una lista ~p ~n",[cmd, CMDid, Args]),
            gen_tcp:send(Socket, Paquete),
            echoResp(Socket);
        {error, closed} ->
            io:format("El cliente cerró la conexión~n")
    end.
