-module(process_access).
-export([start_process/2, send_to_process/2, send_to_process/3]).

start_process(Process_Name, Data) ->
  register(list_to_atom(Process_Name), spawn(process, start, [Data])),
  true.

send_to_process(Process_Name, Cmd) ->
  whereis(list_to_atom(Process_Name)) ! {list_to_atom(Cmd), self()},
  receive
    Data -> Data
  end.

send_to_process(Process_Name, Cmd, Data) ->
  whereis(list_to_atom(Process_Name)) ! {list_to_atom(Cmd), self(), Data},
  receive
    RData -> RData
  end.

