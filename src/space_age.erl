-module(space_age).
-export([ageOn/2]).

% Orbital periods relative to Earth years.
-define(PERIODS,
        #{mercury => 0.2408467,
          venus => 0.61519726,
          earth => 1,
          mars => 1.8808158,
          jupiter => 11.862615,
          saturn => 29.447498,
          uranus => 84.016846,
          neptune => 164.79132}).

-type planet() :: mercury | venus | earth | mars | jupiter | saturn | uranus | neptune.

-spec to_years(number()) -> float().
to_years(Seconds) ->
  Seconds / 31557600.

-spec ageOn(planet(), pos_integer()) -> float().
ageOn(Planet, Seconds) ->
  to_years(Seconds / maps:get(Planet, ?PERIODS)).
