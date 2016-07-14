-module(rna_transcription).
-export([to_rna/1]).

-spec to_rna(string()) -> string().
to_rna(Sequence) ->
  lists:map(fun transcribe/1, Sequence).

-spec transcribe(char()) -> char().
transcribe($A) -> $U;
transcribe($C) -> $G;
transcribe($G) -> $C;
transcribe($T) -> $A.
