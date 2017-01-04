-module(atbash_cipher).
-export([encode/1, decode/1]).

-define(CIPHER,
        lists:zip(lists:seq($a, $z) ++ lists:seq($0, $9),
                  lists:reverse(lists:seq($a, $z)) ++ lists:seq($0, $9))).

encode(S) ->
  space_out([bash(C) || C <- keep_alphanum(string:to_lower(S))]).

decode(S) ->
  [bash(C) || C <- keep_alphanum(S)].

keep_alphanum(S) ->
  re:replace(S, "[^a-z0-9]", "", [global, {return, list}]).

bash(C) ->
  {C, N} = lists:keyfind(C, 1, ?CIPHER),
  N.

space_out(L) when length(L) < 6 ->
  L;
space_out(L) ->
  lists:sublist(L, 5) ++ " " ++ space_out(lists:nthtail(5, L)).
