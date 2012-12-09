-module(process).
-export([start/1]).

start(Data) ->
  receive
    {get, Pid} ->
      io:format("sending data: ~p~n", [Data]),
      Pid ! Data,
      start(Data);
    {stop, _} ->
      ok
  end.
