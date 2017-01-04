-module(luhn).
-export([valid/1, create/1]).

valid(Digits) ->
  Number = to_int_list(Digits),
  Adjusted = adjust(Number, []),
  0 =:= sum(Adjusted) rem 10.

create(Digits) ->
  Number = [0 | to_int_list(Digits)],
  Adjusted = adjust(Number, []),
  CheckDigit = 10 - (sum(Adjusted) rem 10),
  Digits ++ integer_to_list(CheckDigit).

%% Strips non-digits and reverses the value to feed into adjust/2.
to_int_list(Digits) ->
  [D - $0 || D <- lists:reverse(Digits), D >= $0 andalso D =< $9].

%% Applies double_ish/1 to every second digit.
adjust([], Acc) ->
  Acc;
adjust([X, Y | Rest], Acc) ->
  adjust(Rest, [double_ish(Y), X | Acc]);
adjust([X], Acc) ->
  [X | Acc].

%% Double the digit, but subtract 9 if 10 or greater.
double_ish(N) when N =< 4 ->
  N * 2;
double_ish(N) ->
  N * 2 - 9.

sum(IntList) ->
  lists:foldl(fun(E, A) -> A + E end, 0, IntList).