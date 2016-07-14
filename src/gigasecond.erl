-module(gigasecond).
-export([from/1]).

-spec from(calendar:date() | calendar:datetime()) -> calendar:datetime().
from({Year, Month, Day}) ->
  from({{Year, Month, Day}, {0, 0, 0}});
from(DateTime) ->
  Gs = calendar:datetime_to_gregorian_seconds(DateTime) + 1000000000,
  calendar:gregorian_seconds_to_datetime(Gs).