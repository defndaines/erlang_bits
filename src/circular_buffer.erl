-module(circular_buffer).
-behavior(gen_server).

%% Exercism API
-export([create/1, size/1, read/1, write/2, write_attempt/2]).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, { size
               , read_pos
               , write_pos = 1
               , buffer
               }).

%%% API

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% gen_server callbacks

init([]) -> {ok, #state{}}.

handle_call({init, Size}, _From, _State) ->
  {reply, ok, #state{size = Size}};
handle_call(size, _From, State) ->
  {reply, {ok, State#state.size}, State};
handle_call(read, _From, State) when State#state.read_pos =:= undefined ->
  {reply, {error, empty}, State};
handle_call(read, _From, State) ->
  Value = 
  {reply, {ok, Value}, State#state{read_pos = State#state.read_pos + 1}}.
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%% Exercism API
% -export([create/1, size/1, read/1, write/2, write_attempt/2]).

create(Size) ->
  Pid = case start_link() of
          {ok, P} -> P;
          {error, {already_started, P}} -> P
        end,
  gen_server:call(?MODULE, {init, Size}),
  Pid.

size(_Pid) ->
  gen_server:call(?MODULE, size).

read(_Pid) ->
  gen_server:call(?MODULE, read).

write(_Pid, Value) ->
  gen_server:write(?MODULE, {write, Value}).

write_attempt(_Pid, Value) ->
  gen_server:write(?MODULE, {write, Value}).

%%% Internal functions
