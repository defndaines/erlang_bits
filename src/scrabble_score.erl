-module(scrabble_score).
-export([score/1]).

-define(SCORES, #{ $D => 2, $G => 2
                 , $B => 3, $C => 3, $M => 3, $P => 3
                 , $F => 4, $H => 4, $V => 4, $W => 4, $Y => 4
                 , $K => 5
                 , $J => 10, $X => 8
                 , $Q => 10, $Z => 10 }).

score(Word) ->
  lists:foldl(fun(E, Acc) -> Acc + maps:get(E, ?SCORES, 1) end,
              0,
              string:to_upper(Word)).