-module(robot_simulator).
-behavior(gen_server).

%% Exercism API
-export([ create/0
        , place/3
        , direction/1
        , position/1
        , left/1
        , right/1
        , advance/1
        , control/2
        ]).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, robot).

-define(DIRECTIONS, {north, east, south, west}).

-record(state, { direction
               , position = {undefined, undefined}
               }).

%%% API

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% gen_server callbacks

init([]) ->
  {ok, #state{}}.

handle_call(direction, _From, State) ->
  {reply, State#state.direction, State};
handle_call(position, _From, State) ->
  {reply, State#state.position, State};
handle_call({place, Direction, Position}, _From, State) ->
  {reply, ok, State#state{ direction = Direction, position = Position}};
handle_call({turn, Hand}, _From, State) ->
  NewDirection = turn(Hand, State),
  {reply, ok, NewDirection};
handle_call(advance, _From, State) ->
  NewPosition = move(State),
  {reply, ok, NewPosition};
handle_call({control, Command}, _From, State) ->
  NewState = lists:foldl(fun(C, Acc) -> command(C, Acc) end, State, Command),
  {reply, ok, NewState};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%% Exercism API

create() ->
  case gen_server:start_link({local, ?SERVER}, ?MODULE, [], []) of
    {ok, _} -> ?SERVER;
    {error, {already_started, _}} -> ?SERVER
  end.

place(robot, Direction, Position) ->
  gen_server:call(robot, {place, Direction, Position}).

direction(robot) ->
  gen_server:call(robot, direction).

position(robot) ->
  gen_server:call(robot, position).

left(robot) ->
  gen_server:call(robot, {turn, left}).

right(robot) ->
  gen_server:call(robot, {turn, right}).

advance(robot) ->
  gen_server:call(robot, advance).

control(robot, "unknown") ->
  ok;
control(robot, Command) ->
  gen_server:call(robot, {control, Command}).

%%% Internal functions

turn(left, State) when State#state.direction =:= north ->
  State#state{direction = west};
turn(left, State) when State#state.direction =:= west ->
  State#state{direction = south};
turn(left, State) when State#state.direction =:= south ->
  State#state{direction = east};
turn(left, State) when State#state.direction =:= east ->
  State#state{direction = north};
turn(right, State) when State#state.direction =:= north ->
  State#state{direction = east};
turn(right, State) when State#state.direction =:= west ->
  State#state{direction = north};
turn(right, State) when State#state.direction =:= south ->
  State#state{direction = west};
turn(right, State) when State#state.direction =:= east ->
  State#state{direction = south}.

move(State) when State#state.direction =:= north ->
  {X, Y} = State#state.position,
  State#state{position = {X, Y + 1}};
move(State) when State#state.direction =:= east ->
  {X, Y} = State#state.position,
  State#state{position = {X + 1, Y}};
move(State) when State#state.direction =:= south ->
  {X, Y} = State#state.position,
  State#state{position = {X, Y - 1}};
move(State) when State#state.direction =:= west ->
  {X, Y} = State#state.position,
  State#state{position = {X - 1, Y}}.

command($R, State) ->
  turn(right, State);
command($L, State) ->
  turn(left, State);
command($A, State) ->
  move(State).
