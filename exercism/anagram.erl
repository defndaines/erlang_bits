-module(anagram).
-export([find/2]).

-spec find(string(), [string()]) -> string().
find(Word, List) ->
  Normalized = string:to_lower(Word),
  [W || W <- List, is_anagram(Normalized, string:to_lower(W))].

-spec is_anagram(string(), string()) -> boolean().
is_anagram(Word, Word) ->
  % A word is not considered an anagram of itself.
  false;
is_anagram(Word, Candidate) ->
  lists:sort(Word) =:= lists:sort(Candidate).