-module(parse).
-export([resolve/1]).

resolve(Str) ->
  lists:foldl(fun combine/2, [""], Str).

combine($?, Acc) ->
  [E ++ [N] || E <- Acc, N <- "01"];
combine(C, Acc) ->
  [E ++ [C] || E <- Acc].
