-module(trinary).
-export([to_decimal/1]).

to_decimal(Value) ->
  to_decimal(lists:reverse(Value), 0, 0).

to_decimal([], _Pow, Acc) ->
  trunc(Acc);
to_decimal([Digit | _Rest], _Pow, _Acc) when Digit < $0 orelse Digit > $2 ->
  0;
to_decimal([Digit | Rest], Pow, Acc) ->
  to_decimal(Rest, Pow + 1, (Digit - $0) * math:pow(3, Pow) + Acc).