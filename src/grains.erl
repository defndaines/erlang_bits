-module(grains).
-export([square/1, total/0]).

square(Position) ->
  2 bsl (Position - 2).

total() ->
  lists:sum([square(G) || G <- lists:seq(1, 64)]).