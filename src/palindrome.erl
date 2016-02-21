-module(palindrome).

-export([dist/1]).

%% Calculate the number of characters (distance) to a palindrome.
%% Also include the full palindrome in the response.
dist(Word) ->
  Reverse = lists:reverse(Word),
  lists:min([dist(Reverse, Word, {0, []}), dist(Word, Reverse, {0, []})]).

dist([], _, {N, Acc}) ->
  {N, Acc ++ lists:reverse(Acc)};
dist([C], [C | _], {N, Acc}) ->
  {N, Acc ++ [C] ++ lists:reverse(Acc)};
dist([C | Word], [C | Rev], {N, Acc}) ->
  dist(lists:droplast(Word), Rev, {N, Acc ++ [C]});
dist([C | Word], Rev, {N, Acc}) ->
  dist(Word, Rev, {N + 1, Acc ++ [C]}).
