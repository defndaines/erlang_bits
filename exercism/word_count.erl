-module(word_count).
-export([count/1]).

% Regular expression of all invalid characters to be stripped from any phrase.
-define(INVALID, "[^0-9A-Za-z ,_]").

count(Phrase) ->
  Valid = re:replace(Phrase, ?INVALID, "", [global, {return, list}]),
  Lower = string:to_lower(Valid),
  Tokens = string:tokens(Lower, " ,_"),
  Freq = frequencies(Tokens),
  dict:from_list(Freq).

% Using function I'd already written and tested.
% TODO: Convert to using a dict instead of a list.

frequencies(List) ->
  lists:foldl(fun count_into/2, [], List).

count_into(Elem, Acc) ->
  lists:keystore(Elem, 1, Acc, {Elem, proplists:get_value(Elem, Acc, 0) + 1}).