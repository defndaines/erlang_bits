-module(beer_song).
-export([verse/1, sing/1, sing/2]).

verse(Bottles) ->
  current_bottles(Bottles) ++ next_action(Bottles).

sing(Start) ->
  sing(Start, 0).

sing(Start, End) ->
  sing(Start, End, []).

sing(Start, End, Acc) when Start < End ->
  Acc;
sing(Start, End, Acc) ->
  sing(Start - 1, End, Acc ++ verse(Start) ++ "\n").

current_bottles(0) ->
  "No more bottles of beer on the wall, no more bottles of beer.\n";
current_bottles(1) ->
  "1 bottle of beer on the wall, 1 bottle of beer.\n";
current_bottles(Bottles) ->
  io_lib:fwrite("~p bottles of beer on the wall, ~p bottles of beer.~n", [Bottles, Bottles]).

next_action(0) ->
  "Go to the store and buy some more, 99 bottles of beer on the wall.\n";
next_action(1) ->
  "Take it down and pass it around, no more bottles of beer on the wall.\n";
next_action(2) ->
  "Take it down and pass it around, 1 bottle of beer on the wall.\n";
next_action(Bottles) ->
  io_lib:fwrite("Take it down and pass it around, ~p bottles of beer on the wall.~n", [Bottles - 1]).