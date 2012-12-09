-module(process).
-export([start/1]).

start(Data) ->
  receive
    {get, Pid} ->
      Pid ! Data,
      start(Data);
    {stop} ->
      ok
  end.
