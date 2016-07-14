-module(difference_of_squares).
-export([sum_of_squares/1, square_of_sums/1, difference_of_squares/1]).

sum_of_squares(N) ->
  lists:sum([X * X || X <- lists:seq(1, N)]).

square_of_sums(N) ->
  round(math:pow(lists:sum(lists:seq(1, N)), 2)).

difference_of_squares(N) ->
  square_of_sums(N) - sum_of_squares(N).