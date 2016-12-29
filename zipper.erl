-module(zipper).
-export([ new_tree/3
        , empty/0
        , from_tree/1
        , to_tree/1
        , value/1
        , left/1
        , right/1
        , up/1
        , down/1
        , set_value/2
        , set_left/2
        , set_right/2
        ]).

-type item() :: pos_integer() | nil.
-type tree() :: item() | [tree()].
-type path() :: top | { [tree()], path(), [tree()] }.
-type location() :: { tree(), path() }.


-spec new_tree(tree(), tree(), tree()) -> location().
new_tree(Value, Left, Right) ->
  {Value, {Left, top, Right}}.

-spec empty() -> tree().
empty() ->
  [].

-spec from_tree(tree()) -> location().
from_tree(Tree) ->
  {[], Tree}.

-spec to_tree(location()) -> tree().
to_tree({_Path, Tree}) ->
  Tree.

-spec value(location()) -> item().
value({Value, _Path}) ->
  Value.

-spec left(location()) -> location().
left({T, {[L | Left], Up, Right}}) ->
  {L, {Left, Up, [T | Right]}}.
% left(top) ->
  % Left of top, crash!
% left({[], Up, Right}) ->
  % Left of first, crash!

-spec right(location()) -> location().
right({T, {Left, Up, [R | Right]}}) ->
  {R, {[T | Left], Up, Right}}.
% right(top) ->
  % Right of top, crash!
% right(_) ->
  % Right of last, crash!

-spec up(location()) -> location().
up({T, {Left, Up, Right}}) ->
  {lists:reverse(Left) ++ [T | Right], Up}.
% up({T, {Left, top, Right}}) ->
  % Up of top, crash!

-spec down(location()) -> location().
down({[T | Trees], Path}) ->
  {T, {[], Path, Trees}}.
% down(_) ->
  % Down of item or empty, crash!

-spec set_value(location(), item()) -> location().
set_value({_Tree, Path}, Value) ->
  {Value, Path}.

-spec set_left(location(), tree()) -> location().
set_left({Tree, {_Left, Up, Right}}, Left) ->
  {Tree, {Left, Up, Right}}.

-spec set_right(location(), tree()) -> location().
set_right({Tree, {Left, Up, _Right}}, Right) ->
  {Tree, {Left, Up, Right}}.