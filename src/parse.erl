-module(parse).
-export([resolve/1]).

resolve(Str) ->
  resolve(Str, [""]).

resolve([], Acc) -> Acc;
resolve([$? | Rest], Acc) ->
  resolve(Rest, [E ++ [N] || E <- Acc, N <- "01"]);
resolve([C | Rest], Acc) ->
  resolve(Rest, [E ++ [C] || E <- Acc]).
