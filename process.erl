-module(process).
-export([start/1]).

start(Data) ->
  receive
    {get, Pid} ->
      io:format("sending data: ~p~n", [Data]),
      Pid ! Data,
      start(Data);
    {get, Pid, Key} ->
      io:format("sending data @ key: ~p~n", [Key]),
      case lists:keyfind(Key, 1, tuple_to_list(Data)) of
        {_K, V} -> Pid ! V;
        false -> Pid ! null
      end,
      start(Data);
    {set, Pid, Data2} ->
      io:format("setting data: ~p~n", [Data2]),
      Pid ! {Data, Data2},
      start(Data2);
    {set, Pid, Key, Val} ->
      io:format("setting data @ key: ~p~n", [Key]),
      Result = lists:keydelete(Key, 1, tuple_to_list(Data)),
      Pid ! {Key, Val},
      start(list_to_tuple(Result ++ [{Key, Val}]));
    {stop, Pid} ->
      Pid ! ok
  end.
