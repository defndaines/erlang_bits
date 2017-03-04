-module(rotational_cipher).
-export([encrypt/2, decrypt/2, test_version/0]).

encrypt(Text, Rot) ->
  [rotate(C, Rot) || C <- Text].

decrypt(Text, Rot) ->
  [rotate(C, (26 - Rot)) || C <- Text].

rotate(C, Rot) when $a =< C andalso C =< $z ->
  ((C - $a) + Rot) rem 26 + $a;
rotate(C, Rot) when $A =< C andalso C =< $Z ->
  ((C - $A) + Rot) rem 26 + $A;
rotate(C, _Rot) ->
  C.

test_version() -> 1.
