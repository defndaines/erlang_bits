-module(bob).
-export([response_for/1]).

response_for(Phrase) ->
  respond(string:strip(Phrase)).

respond("") ->
  "Fine. Be that way!";
respond(Phrase) ->
  case {shout(Phrase), question(Phrase)} of
    {shout, _} -> "Whoa, chill out!";
    {_, question} -> "Sure.";
    _Otherwise -> "Whatever."
  end.

shout(Phrase) ->
  case {string:to_upper(Phrase), re:replace(Phrase, "[A-Za-z]", "")} of
    {Phrase, Phrase} -> statement;
    {Phrase, _} -> shout;
    {_, _} -> statement
  end.

question(Phrase) ->
  case re:run(Phrase, "\\?$") of
    {match, _Position} -> question;
    nomatch -> statement
  end.