-module(process_access).
-export([start_process/2, send_to_process/2, send_to_process/3, send_to_process/4]).

start_process(Process_Name, Data) ->
  case whereis(list_to_atom(Process_Name)) of
    undefined ->
      {ok, _} = gen_server:start_link({local, list_to_atom(Process_Name)}, process, Data, []),
      true;
    _Pid -> false
  end.

send_to_process(Process_Name, Cmd) ->
  case whereis(list_to_atom(Process_Name)) of
    _Pid when is_pid(_Pid) -> gen_server:call(_Pid, list_to_atom(Cmd));
    undefined -> error
  end.

send_to_process(Process_Name, Cmd, Data) ->
  case whereis(list_to_atom(Process_Name)) of
    _Pid when is_pid(_Pid) -> gen_server:call(_Pid, {list_to_atom(Cmd), Data});
    undefined -> error
  end.

send_to_process(Process_Name, Cmd, Data, Data2) ->
  case whereis(list_to_atom(Process_Name)) of
    _Pid when is_pid(_Pid) -> gen_server:call(_Pid, {list_to_atom(Cmd), Data, Data2});
    undefined -> error
  end.
