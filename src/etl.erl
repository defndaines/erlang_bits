-module(etl).
-export([transform/1]).

transform(Scores) ->
  % Accumulate values into a dict, but tests expect a list of tuples.
  Dict = transform(Scores, dict:new()),
  % Tests expect keys to be sorted alphabetically.
  lists:keysort(1, lists:keymap(fun map/1, 2, dict:to_list(Dict))).

transform([], Acc) ->
  Acc;
transform([{Key, Values} | Tail], Acc) ->
  transform(Tail, transform(Values, Key, Acc)).

transform([], _Value, Acc) ->
  Acc;
transform([Word | Rest], Value, Acc) ->
  transform(Rest, Value, dict:append(string:to_lower(Word), Value, Acc)).

%% Final clean up of values. Tests expect integers or strings as a result.
map([Value]) when is_number(Value) ->
  Value;
map(Value) ->
  lists:flatten(Value).
