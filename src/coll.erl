%%% Collections module.
%%%
%%% Collection functions, some inspired by Clojure.

-module(coll).
-export([frequencies/1]).

-type freq_list(T) :: proplists:proplist({T, non_neg_integer()}).

%% Returns a proplist of distinct items in a list to the number of times
%% they appear.
%%
%% Examples:
%% > coll:frequencies([red, blue, green, green, red, red, green, red, green]).
%% [{red, 4}, {blue, 1}, {green, 4}]
%% > [{[Letter], Count} || {Letter, Count} <- coll:frequencies("cellar door")].
%% [{"c", 1}, {"e", 1}, {"l", 2}, {"a", 1}, {"r", 2}, {" ", 1}, {"d", 1}, {"o", 2}]
-spec frequencies(list(T)) -> freq_list(T).
frequencies(List) ->
  lists:foldl(fun count_into/2, [], List).

-spec count_into(T, freq_list(T)) -> freq_list(T).
count_into(Elem, Acc) ->
  lists:keystore(Elem, 1, Acc, {Elem, proplists:get_value(Elem, Acc, 0) + 1}).

% Consider a partition-by?
% e.g., partition_by(fun(X) -> X rem 2 =:= 0 end, [2, 4, 6, 1, 3, 8, 10, 9, 7])
%            ... [[2, 4, 6], [1, 3], [8, 10], [9, 7]]

