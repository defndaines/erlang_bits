-module(rna_transcription).
-export([to_rna/1]).

-spec to_rna(string()) -> string().
to_rna(Sequence) ->
  transcribe(Sequence, "").

-spec transcribe(string(), string()) -> string().
transcribe("", Transcription) ->
  Transcription;
transcribe("G" ++ Rest, Transcription) ->
  transcribe(Rest, Transcription ++ "C");
transcribe("C" ++ Rest, Transcription) ->
  transcribe(Rest, Transcription ++ "G");
transcribe("T" ++ Rest, Transcription) ->
  transcribe(Rest, Transcription ++ "A");
transcribe("A" ++ Rest, Transcription) ->
  transcribe(Rest, Transcription ++ "U").
