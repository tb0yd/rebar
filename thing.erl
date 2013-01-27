-module(thing).
-behaviour(gen_server).
-export([init/1, handle_call/3, terminate/2, handle_cast/2, code_change/3, handle_info/2]).

init(_) ->
  {ok, []}.

handle_call([puts, Bin], _From, State) ->
  {reply, true, [State,Bin,<<"\n">>]};
  
handle_call([result], _From, State) ->
  Result = list_to_binary([State, <<"Fun for everyone!\n">>]),
  {reply, Result, State}.

handle_cast([initialize_args|Args], _) ->
  {noreply, Args};

handle_cast(_, State) ->
  {noreply, State}.

handle_info(_, State) ->
  {noreply,State}.

terminate(_, _) ->
  ok.

code_change(_, State, _) ->
  {ok, State}.

