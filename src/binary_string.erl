-module(binary_string).
-export([to_decimal/1]).

to_decimal(String) ->
  lists:foldl(fun shift/2, 0, String).

shift($0, Acc) ->
  Acc bsl 1;
shift($1, Acc) ->
  Acc bsl 1 + 1; 
shift(_, _Acc) ->
  0.
