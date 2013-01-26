-module(brerrecv).
-export([start/0, handle/1]).

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

  % process it
  Json = jiffy:decode(Bin),
  [A, B, C, D] = brerparse:parse(Json),

  % get result
  Result = brergs:send(A, B, C, D),

  % send result back
  gen_tcp:send(Sock, jiffy:encode({[{result, Result}]})),
  ok = gen_tcp:close(Sock).

