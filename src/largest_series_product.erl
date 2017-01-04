-module(largest_series_product).
-export([from_string/2]).

-type string_of_numbers() :: [48..57].

-spec from_string(string_of_numbers(), non_neg_integer()) -> non_neg_integer().
from_string(_, 0) -> 1;
from_string(String, N) when length(String) >= N ->
  Ints = [I - 48 || I <- String],
  Partitions = [lists:sublist(Ints, X, N)
                || X <- lists:seq(1, length(String) - N + 1)],
  lists:foldl(fun max_product/2, 0, Partitions).

-spec product([integer()]) -> integer().
product(L) ->
  lists:foldl(fun(X, Prod) -> X * Prod end, 1, L).

-spec max_product([integer()], integer()) -> integer().
max_product(L, Acc) ->
  Product = product(L),
  case Product > Acc of
    true -> Product;
    _ -> Acc
  end.
