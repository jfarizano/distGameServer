-module(echoCli).
-include("echoOpts.hrl").
%% -record(punto,{x, y}).
-export([cliente/0]).

cliente()->
    {ok , Socket} = gen_tcp:connect("localhost"
                                   , ?Puerto
                                   , [ binary
                                     , {packet, 0}
                                     , {active,false}]),
    Test = {cmd, "Hola", ["Santi", "Punto"]},
    gen_tcp:send(Socket, term_to_binary(Test)),
    case gen_tcp:recv(Socket, 0) of
        {ok , Paquete} ->
            io:format("Llega: ~p ~n",[binary_to_term(Paquete)]),
            gen_tcp:close(Socket);
        {error, Razon} ->
            io:format("Error por ~p ~n",[Razon])
    end.

