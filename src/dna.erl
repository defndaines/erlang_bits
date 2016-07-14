-module(dna).
-export([count/2, nucleotide_counts/1]).

% Accumulator uses the integer values and must be converted at the end.
% Also, this ordering ensures that the tests pass.
-define(COUNT_MAP, [{65, 0}, {84, 0}, {67, 0}, {71, 0}]).

-type nucleotide() :: 65 | 67 | 71 | 84.
-type dna() :: [nucleotide()].
-type count_map() :: [{nucleotide(), non_neg_integer()}].
-type result_map() :: [{string(), non_neg_integer()}].

-spec count(dna(), nucleotide()) -> non_neg_integer().
count(Sequence, Nucleotide) ->
  case proplists:get_value(Nucleotide, nucleotide_counts(Sequence)) of
    undefined -> error("Invalid nucleotide");
    Value -> Value
  end.

-spec nucleotide_counts(dna()) -> result_map().
nucleotide_counts(Sequence) ->
  nucleotide_counts(Sequence, ?COUNT_MAP).

-spec nucleotide_counts(dna(), count_map()) -> result_map().
nucleotide_counts([], Acc) ->
  [{[Nucleotide], Count} || {Nucleotide, Count} <- Acc];
nucleotide_counts([N | Rest], Acc) ->
  case proplists:get_value(N, Acc) of
    undefined -> nucleotide_counts(Rest, Acc);
    Value -> nucleotide_counts(Rest, lists:keyreplace(N, 1, Acc, {N, Value + 1}))
  end.

%% Test cases are order sensitive, so these won't pass.
% frequencies(List) ->
%   lists:foldl(fun count_into/2, [], List).
%
% count_into(Elem, Acc) ->
%   lists:keystore(Elem, 1, Acc, {Elem, proplists:get_value(Elem, Acc, 0) + 1}).