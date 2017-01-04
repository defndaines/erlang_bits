-module(triangle).
-export([kind/3]).

kind(A, B, C) ->
  [X, Y, Z] = lists:sort([A, B, C]),
  kind({X, Y, Z}).

kind({A, _, _}) when A =< 0 ->
  {error, "all side lengths must be positive"};
kind({A, A, A}) ->
  equilateral;
kind({A, B, C}) when (A + B) =< C ->
  {error, "side lengths violate triangle inequality"};
kind({_, A, A}) when A > 0 ->
  isosceles;
kind({_, _, _}) ->
  scalene.