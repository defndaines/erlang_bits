-module(parse).
-export([resolve/1]).

resolve(Str) ->
  [lists:reverse(binary_to_list(E)) || E <- resolve(list_to_binary(Str), [<<>>])].

resolve(<<>>, Acc) -> Acc;
resolve(<<$?, Rest/binary>>, Acc) ->
  resolve(Rest, [<<N, E/binary>> || E <- Acc, N <- "01"]);
resolve(<<C, Rest/binary>>, Acc) ->
  resolve(Rest, [<<C, E/binary>> || E <- Acc]).
