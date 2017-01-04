-module(meetup).
-export([schedule/4]).

-type year() :: 2013..2036.
-type month() :: 1..12.
-type day_of_week() :: monday | tuesday | wednesday | thursday | friday | saturday | sunday.
-type period() :: first | second | third | fourth | last | teenth.

-spec schedule(year(), month(), day_of_week(), period()) -> {year(), month(), 1..31}.
schedule(Year, Month, DayOfWeek, Period) ->
  PeriodStart = check_from(Period, Year, Month),
  DoW = calendar:day_of_the_week({Year, Month, PeriodStart}),
  Day = day_of_week(DayOfWeek),
  {Year, Month, PeriodStart + (7 + (Day - DoW)) rem 7}.

-spec day_of_week(day_of_week()) -> 1..7.
day_of_week(monday) -> 1;
day_of_week(tuesday) -> 2;
day_of_week(wednesday) -> 3;
day_of_week(thursday) -> 4;
day_of_week(friday) -> 5;
day_of_week(saturday) -> 6;
day_of_week(sunday) -> 7.

-spec check_from(period(), year(), month()) -> 1..25.
check_from(first, _Year, _Month) -> 1;
check_from(second, _Year, _Month) -> 8;
check_from(third, _Year, _Month) -> 15;
check_from(fourth, _Year, _Month) -> 22;
check_from(teenth, _Year, _Month) -> 13;
check_from(last, Year, Month) ->
  calendar:last_day_of_the_month(Year, Month) - 6.
