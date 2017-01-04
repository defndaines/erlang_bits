-module(parallel_letter_frequency).
-export([dict/1, letters/2]).

-type char_count() :: dict:dict(char(), non_neg_integer()).

-spec dict([string()]) -> char_count().
dict(L) ->
  Pids = [spawn(?MODULE, letters, [self(), Word]) || Word <- L],
  lists:foldl(fun merge_results/2, dict:new(), Pids).

-spec letters(pid(), string()) -> {pid(), char_count()}.
letters(Pid, Word) ->
  Pid ! {self(), lists:foldl(fun inc_letter/2, dict:new(), Word)}.

-spec inc_letter(char(), char_count()) -> char_count().
inc_letter(Char, Dict) ->
  dict:update_counter(Char, 1, Dict).

-spec merge_results(pid(), char_count()) -> char_count().
merge_results(Pid, Dict) ->
  receive
    {Pid, Results} ->
      dict:merge(fun(_K, V1, V2) -> V1 + V2 end, Dict, Results)
  end.