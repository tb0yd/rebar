-module(process_access).
-export([start_process/2, send_to_process/2, send_to_process/3, send_to_process/4]).

start_process(Process_Name, Data) ->
  case whereis(list_to_atom(Process_Name)) of
    undefined ->
      register(list_to_atom(Process_Name), spawn(process, start, [Data])),
      true;
    _Pid -> false
  end.

send_to_process(Process_Name, Cmd) ->
  case whereis(list_to_atom(Process_Name)) of
    _Pid when is_pid(_Pid) ->
      _Pid ! {list_to_atom(Cmd), self()},
      receive
        Data -> Data
      end;
    undefined -> error
  end.

send_to_process(Process_Name, Cmd, Data) ->
  whereis(list_to_atom(Process_Name)) ! {list_to_atom(Cmd), self(), Data},
  receive
    RData -> RData
  end.

send_to_process(Process_Name, Cmd, Data, Data2) ->
  whereis(list_to_atom(Process_Name)) ! {list_to_atom(Cmd), self(), Data, Data2},
  receive
    RData -> RData
  end.

