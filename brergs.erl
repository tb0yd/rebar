-module(brergs).
-export([send/4]).

-define(TIMEOUT, 250).

send(start_link, Module, Name, Args) ->
  case gen_server:start_link({local, Name}, Module, Args, [{timeout, ?TIMEOUT}]) of
    {ok,_} -> {ok, true};
    {error, {already_started, _}} -> {ok, true};
    {error, timeout} -> {error, timeout}
  end;

send(call, Name, Fun, Args) ->
  try
    {ok, gen_server:call(Name, [Fun|Args], ?TIMEOUT)}
  catch
    exit:{timeout, _} -> {error, timeout}
  end;
  
send(cast, Name, Fun, Args) ->
  gen_server:cast(Name, [Fun|Args]).

