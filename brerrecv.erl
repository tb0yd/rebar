-module(brerrecv).
-export([start/0, handle/1]).

-define(PING_TIMEOUT, 300).

start() ->
  {ok, LSock} = gen_tcp:listen(5500, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
  inet:setopts(LSock, [{recbuf,16384}]), % arbitrary value -- split up & iterate to allow higher? -TB
  loop(LSock).

loop(LSock) ->
  {ok, Sock} = gen_tcp:accept(LSock),
  spawn(brerrecv, handle, [Sock]),  
  loop(LSock).

handle(Sock) ->
  % read the request from the socket
  {ok, Bin} = gen_tcp:recv(Sock, 0),
  io:format("Bin: ~p~n", [Bin]),
  Json = jiffy:decode(Bin),
  io:format("JSON: ~p~n", [Json]),

  % TODO: pull this part out into separate module
  Result = case Json of
    [<<"init">>, BModule, BName, Args] ->
      Name = list_to_atom(binary_to_list(BName)),
      Module = list_to_atom(binary_to_list(BModule)),

      %% TODO: handle errors
      {ok, _} = gen_server:start_link({local, Name}, Module, Args, []),

      true; %% the actual result displayed by Brer::ErlangProcess
    [<<"call">>, BName, BFun, Args] ->
      Name = list_to_atom(binary_to_list(BName)),
      Fun = list_to_atom(binary_to_list(BFun)),
      gen_server:call(Name, [Fun|Args], ?PING_TIMEOUT);
    [<<"cast">>, BName, BFun, Args] ->
      Name = list_to_atom(binary_to_list(BName)),
      Fun = list_to_atom(binary_to_list(BFun)),
      gen_server:cast(Name, [Fun|Args])
  end,

  % send the ressult
  io:format("Result: ~p~n", [Result]),
  gen_tcp:send(Sock, jiffy:encode({[{result, Result}]})),
  
  % send the response
  ok = gen_tcp:close(Sock).

