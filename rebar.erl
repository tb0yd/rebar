-module(rebar).
-export([start/0, handle/1]).

start() ->
  {ok, LSock} = gen_tcp:listen(5500, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
  inet:setopts(LSock, [{recbuf,16384}]), % arbitrary value -- split up & iterate to allow higher? -TB
  loop(LSock).
    
loop(LSock) ->
  {ok, Sock} = gen_tcp:accept(LSock),
  spawn(rebar, handle, [Sock]),  
  loop(LSock).
  
handle(Sock) ->
  % read the request from the socket
  {ok, Bin} = gen_tcp:recv(Sock, 0),
  {ok, Json} = json:decode_string(binary_to_list(Bin)),
  
  % pull the request apart (what kind of gen_server command)
  case parse(Json) of
    {call, {Msg, Params}} ->
      [Process_Name, Cmd] = string:tokens(Msg, ":"),
      io:format("~p:~p(~p)~n", [Process_Name, Cmd, Params]),

      % call the function
      Result = safe_apply(Process_Name, Cmd, Params),
      gen_tcp:send(Sock, json:encode(json:obj_from_list([{"result", element(2, Result)}, {"error", null}])));
    {start_link, Process_Name} ->
      case whereis(Process_Name) of
        Pid when is_pid(Pid) ->
          gen_tcp:send(Sock, json:encode(json:obj_from_list([{"result", "already started"}, {"error", null}])));
        undefined ->
          gen_server:start_link({local, Process_Name}, process, [], []),
          gen_tcp:send(Sock, json:encode(json:obj_from_list([{"result", "ok"}, {"error", null}])))
      end
  end,
  
  % send the response
  ok = gen_tcp:close(Sock).

parse(Json) ->
  {json_object, Body} = Json,
  case lists:keysearch("start_process", 1, Body) of
    {value, {"start_process", Process_Name}} ->
      {start_link, list_to_atom(Process_Name)};
    false ->
      {value, {"method", Method}} = lists:keysearch("method", 1, Body),
      {value, {"params", Params}} = lists:keysearch("params", 1, Body),
      {call, {Method, Params}}
  end.

safe_apply(Process_Name, Cmd, Params) ->
  try
    case whereis(list_to_atom(Process_Name)) of
      Pid when is_pid(Pid) ->
        {good, gen_server:call(Pid, list_to_tuple([list_to_atom(Cmd) | tuple_to_list(Params)]))};
      undefined ->
        {bad, process_not_started}
    end
  catch
    _:_ -> {bad, error}
  end.


