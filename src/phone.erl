-module(phone).
-export([number/1, areacode/1, pretty_print/1]).

-define(VALID_COUNT, 10).
-define(INVALID, lists:duplicate(?VALID_COUNT, $0)).

-spec number(string()) -> string().
number(Candidate) ->
  Digits = [D || D <- Candidate, D >= $0 andalso D =< $9],
  case {length(Digits), Digits} of
    {?VALID_COUNT, _} -> Digits;
    {?VALID_COUNT + 1, [$1 | Rest]} -> Rest;
    _Otherwise -> ?INVALID
  end.

-spec areacode(string()) -> string().
areacode(Digits) ->
  lists:sublist(Digits, 3).

-spec pretty_print(string()) -> string().
pretty_print(Digits) ->
  Number = number(Digits),
  lists:concat(["(", lists:sublist(Number, 3), ") ", lists:sublist(Number, 4, 3), "-", lists:sublist(Number, 7, 4)]).
