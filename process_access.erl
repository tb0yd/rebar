-module(process_access).
-export([start_process/2, send_to_process/2]).

start_process(Process_Name, Data) ->
  register(list_to_atom(Process_Name), spawn(process, start, [Data])),
  true.

send_to_process(Process_Name, Cmd) ->
  list_to_atom(Process_Name) ! Cmd.
