-module(test).
-export([test/0, master/0]).

test() ->
  MasterPid = spawn(?MODULE, master, []),
  register(master, MasterPid),
  MasterPid.

master() ->
  receive
    {PID, Node} -> io:format("~p ~p ~n", [PID, Node]),
                   {PID, Node} ! "Hola",
                   master();
    Mensaje -> io:format("~p ~n", [Mensaje]),
               master()
  end.
