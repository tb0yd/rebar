%% Take a list returned from Jiffy and convert
%% binary strings into atoms.
-module(brerparse).
-export([parse/1]).

parse([Cmd, Arg2, Arg3, Arg4]) ->
  R1 = list_to_atom(binary_to_list(Cmd)),
  R2 = list_to_atom(binary_to_list(Arg2)),
  R3 = list_to_atom(binary_to_list(Arg3)),
  [R1, R2, R3, Arg4].
