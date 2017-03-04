-module(all_your_base).
-export([convert/3, test_version/0]).

convert(_Ns, From, _To) when From < 2 ->
  {error, invalid_src_base};
convert(_Ns, _From, To) when To < 2 ->
  {error, invalid_dst_base};
convert(Ns, From, To) ->
  to_digits(from_digits(Ns, 0, From), [], To).

to_digits({error, Reason}, _, _) ->
  {error, Reason};
to_digits(0, Ds, _Base) ->
  {ok, Ds};
to_digits(N, Ds, Base) ->
  to_digits(N div Base, [N rem Base | Ds], Base).

from_digits([], N, _Base) -> N;
from_digits([D | _Rest], _N, _Base) when D < 0 ->
  {error, negative};
from_digits([D | _Rest], _N, Base) when D >= Base ->
  {error, not_in_base};
from_digits([D | Rest], N, Base) ->
  from_digits(Rest, N * Base + D, Base).

test_version() -> 1.
