-module(allergies).
-export([allergies/1, isAllergicTo/2]).

-define(ALLERGIES, [eggs, peanuts, shellfish, strawberries, tomatoes, chocolate, pollen, cats]).

allergies(Flag) ->
  <<Cats:1, Pollen:1, Chocolate:1, Tomatoes:1, Strawberries:1, Shellfish:1, Peanuts:1, Eggs:1>> = <<Flag:8/integer>>,
  Flags = [Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats],
  Allergens = lists:zipwith(fun parse/2, Flags, ?ALLERGIES),
  lists:filter(fun(A) -> A =/= not_allergic end, Allergens).

isAllergicTo(Allergen, Flag) ->
  lists:member(Allergen, allergies(Flag)).

parse(1, Allergen) -> Allergen;
parse(0, _Allergen) -> not_allergic.
