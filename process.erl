%% module for treating creating a tree of processes to model incoming
%% JSON objects
-module(process).
-export([start/1]).

%% TODO: use gen_server?
start(Data) ->
  loop(process_data(Data)).

loop(Old_Data) ->
  receive
    {get, Pid} ->
      Displayed_Data = display_data(Old_Data),
      io:format("sending data: ~p~n", [Displayed_Data]),
      Pid ! Displayed_Data,
      loop(Old_Data);
    {set, Pid, Unprocessed_Data} ->
      Data = process_data(Unprocessed_Data),
      Displayed_Data = display_data(Data),
      io:format("setting data: ~p~n", [Displayed_Data]),
      Pid ! {display_data(Old_Data), Displayed_Data},
      loop(Data);
    {stop, Pid} ->
      Pid ! ok
  end.

process_data(Data) ->
  process_data(Data, " ").

%% suboptimal way of processing jsons inside lists
process_data({json_object, JsonList}, Sep) ->
  store_list(JsonList, [], Sep);
process_data(Data, Sep) when is_tuple(Data) ->
  process_data(tuple_to_list(Data), [], Sep);
process_data(Data, _) ->
  Data.

process_data([], Result, _) ->
  list_to_tuple(lists:reverse(Result));
process_data([{json_object, JsonList}|T], Result, Sep) ->
  NewSep = Sep ++ integer_to_list(length(Result)) ++ " ",
  process_data(T, [store_list(JsonList, [], NewSep)|Result], Sep);
process_data([H|T], Result, Sep) when is_tuple(H) ->
  NewSep = Sep ++ integer_to_list(length(Result)) ++ " ",
  process_data(T, [process_data(H, NewSep)|Result], Sep);
process_data([H|T], Result, Sep) ->
  process_data(T, [process_data(H, Sep)|Result], Sep).

display_data({json_object, Data}) ->
  get_json(Data, []);
display_data(Data) when is_tuple(Data) ->
  display_data(tuple_to_list(Data), []);
display_data(Data) ->
  Data.

display_data([], Result) ->
  list_to_tuple(lists:reverse(Result));
display_data([{json_object, Data}|T], Result) ->
  display_data(T, [get_json(Data, [])|Result]);
display_data([H|T], Result) ->
  display_data(T, [display_data(H)|Result]).

%% Iterate through list: if tuple, register new process name,
%% spawn new process at the name, set the data at the process,
%% return rest of list.
store_list([], Result, _) ->
  {json_object, Result};

store_list([H|T], Result, Sep) when is_tuple(H) ->
  { Data_Name, Data } = H,
  { registered_name, Parent_Proc_Name } = lists:keyfind(registered_name, 1, process_info(self())),

  Child_Proc_Name = atom_to_list(Parent_Proc_Name) ++ Sep ++ Data_Name,

  case lists:member(list_to_atom(Child_Proc_Name), registered()) of
    true ->
      list_to_atom(Child_Proc_Name) ! {set, self(), Data},
      receive
        _Data -> ok
      end;
    false ->
      Proc = spawn(process, start, [Data]),
      register(list_to_atom(Child_Proc_Name), Proc)
  end,
  store_list(T, [Child_Proc_Name|Result], Sep).

%% parse a list of process names and return a json object matching the
%% values to the process names.
get_json([], Result) ->
  {json_object, Result};

get_json([H|T], Result) ->
  list_to_atom(H) ! {get, self()},
  Json_Key = lists:last(string:tokens(H, " ")),
  receive
    Data ->
      get_json(T, [{Json_Key, Data}|Result])
  end.

