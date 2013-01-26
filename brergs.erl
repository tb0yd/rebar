-module(brergs).
-export([send/4]).

-define(PING_TIMEOUT, 300).

send(start_link, Module, Name, Args) ->
  %% TODO: handle errors
  {ok, _} = gen_server:start_link({local, Name}, Module, Args, []),

  true; %% the actual result displayed by Brer::ErlangProcess

send(call, Name, Fun, Args) ->
  gen_server:call(Name, [Fun|Args], ?PING_TIMEOUT);
  
send(cast, Name, Fun, Args) ->
  gen_server:cast(Name, [Fun|Args]).

