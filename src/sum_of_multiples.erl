-module(sum_of_multiples).
-export([sumOfMultiplesDefault/1, sumOfMultiples/2]).

-spec sumOfMultiplesDefault(pos_integer()) -> non_neg_integer().
sumOfMultiplesDefault(UpTo) ->
  sumOfMultiples([3, 5], UpTo).

-spec sumOfMultiples([pos_integer()], pos_integer()) -> non_neg_integer().
sumOfMultiples(Multiples, UpTo) ->
  Matches = [I || I <- lists:seq(1, UpTo - 1), any_multiple(I, Multiples)],
  sum(Matches).

-spec sum([number()]) -> number().
sum(List) ->
  lists:foldl(fun(X, Y) -> X + Y end, 0, List).

%% Predicate which returns whether I is a multiple of any value in Multiples.
-spec any_multiple(integer(), [integer()]) -> boolean().
any_multiple(I, Multiples) ->
  lists:any(fun(M) -> I rem M =:= 0 end, Multiples).
