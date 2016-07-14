-module(strain).
-export([keep/2, discard/2]).

-spec keep(fun((T) -> boolean()), [T]) -> [T].
keep(Predicate, List) ->
  filter(Predicate, List, []).

-spec discard(fun((T) -> boolean()), [T]) -> [T].
discard(Predicate, List) ->
  filter(fun(Elem) -> not Predicate(Elem) end, List, []).

-spec filter(fun((T) -> boolean()), [T], [T]) -> [T].
filter(_Predicate, [], Acc) ->
  lists:reverse(Acc);
filter(Predicate, [Head | Tail], Acc) ->
  case Predicate(Head) of
    true -> filter(Predicate, Tail, [Head | Acc]);
    false -> filter(Predicate, Tail, Acc)
  end.