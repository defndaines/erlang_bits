-module(clock).
-export([create/2, is_equal/2, minutes_add/2, minutes_delete/2, to_string/1]).

-type hour() :: 0..23.
-type minute() :: 0..59.
-type clock() :: {hour(), minute()}.

-spec create(hour(), minute()) -> clock().
create(Hour, Minute) when Hour >= 0 andalso Hour < 24 andalso Minute >= 0 andalso Minute < 60 ->
  {Hour, Minute}.

-spec is_equal(clock(), clock()) -> boolean().
is_equal(Left, Right) ->
  Left =:= Right.

-spec minutes_add(clock(), integer()) -> clock().
minutes_add({H, M}, Minutes) ->
  {HourOffset, Minute} = adjust_minutes(M + Minutes),
  Hour = adjust_hours(H + HourOffset),
  {Hour, Minute}.

-spec minutes_delete(clock(), integer()) -> clock().
minutes_delete(Clock, Minutes) ->
  minutes_add(Clock, -Minutes).

-spec to_string(clock()) -> string().
to_string({Hour, Minute}) ->
  lists:flatten(io_lib:format("~2..0w:~2..0w", [Hour, Minute])).

-spec adjust_minutes(integer()) -> {integer(), minute()}.
adjust_minutes(Minutes) when Minutes < 0 ->
  {Minutes div 60 - 1, 60 + (Minutes rem 60)};
adjust_minutes(Minutes) ->
  {Minutes div 60, Minutes rem 60}.

-spec adjust_hours(integer()) -> hour().
adjust_hours(Hours) when Hours < 0 ->
  24 + (Hours rem 24);
adjust_hours(Hours) ->
  Hours rem 24.