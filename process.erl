%% module for treating creating a tree of processes to model incoming
%% JSON objects
-module(process).
-behaviour(gen_server).

-export([init/1, handle_call/3, terminate/2, handle_cast/2, code_change/3, handle_info/2]).

init(Data) ->
  {ok, process_data(Data)}.

handle_call({get}, _From, State) ->
  Displayed_Data = display_data(State),
  io:format("sending data: ~p~n", [Displayed_Data]),
  {reply, Displayed_Data, State};

handle_call({set, NewState}, _From, State) ->
  Data = process_data(NewState),
  Displayed_Data = display_data(Data),
  Displayed_Old_Data = display_data(State),
  io:format("setting data: ~p~n", [Displayed_Data]),
  {reply, {Displayed_Old_Data, Displayed_Data}, Data};

handle_call({stop}, _From, State) ->
  {stop,stopped,ok,State}.

handle_cast(_, State) ->
  {noreply, State}.

handle_info(_, State) ->
  {noreply,State}.

code_change(_, State, _) ->
  {ok, State}.

terminate(Reason, _) ->
  io:format("stopping...~p~n", [Reason]).

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
      {ok, _} = gen_server:start_link({local, list_to_atom(Child_Proc_Name)}, process, Data, [])
  end,
  store_list(T, [Child_Proc_Name|Result], Sep).

%% parse a list of process names and return a json object matching the
%% values to the process names.
get_json([], Result) ->
  {json_object, Result};

get_json([H|T], Result) ->
  Json_Key = lists:last(string:tokens(H, " ")),
  Data = gen_server:call(whereis(list_to_atom(H)), {get}),
  get_json(T, [{Json_Key, Data}|Result]).
