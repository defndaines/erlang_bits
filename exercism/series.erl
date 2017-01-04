-module(series).
-export([from_string/2]).

from_string(N, S) ->
  [string:sub_string(S, X, X + N - 1) || X <- lists:seq(1, length(S) - N + 1)].
