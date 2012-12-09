-module(process).
-export([start/1]).

start(Data) ->
  receive
    {get, Pid} ->
      io:format("sending data: ~p~n", [Data]),
      Pid ! Data,
      start(Data);
    {set, Pid, Data2} ->
      io:format("setting data: ~p~n", [Data2]),
      Pid ! {Data, Data2},
      start(Data2);
    {stop, Pid} ->
      Pid ! ok
  end.
