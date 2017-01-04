-module(circular_buffer).
-behavior(gen_server).

%% Exercism API
-export([create/1, size/1, read/1, write/2, write_attempt/2]).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, { read_pos = 0 :: integer()
               , write_pos = 0 :: integer()
               , buffer :: array:array(term())
               }).

%%% API

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% gen_server callbacks

init([]) -> {ok, #state{}}.

handle_call({init, Size}, _From, _State) ->
  {reply, ok, #state{buffer = array:new(Size)}};
handle_call(size, _From, State) ->
  {reply, {ok, array:size(State#state.buffer)}, State};
handle_call(read, _From, State) ->
  {Reply, NewState} = buffer_read(State),
  {reply, Reply, NewState};
handle_call({write, Value}, _From, State) ->
  {Reply, NewState} = buffer_write(Value, State),
  {reply, Reply, NewState};
handle_call({write_attempt, Value}, _From, State) ->
  {Reply, NewState} = case array:get(State#state.write_pos, State#state.buffer) of
                        undefined -> buffer_write(Value, State);
                        _ -> {{error, full}, State}
                      end,
  {reply, Reply, NewState};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%% Exercism API

-spec create(integer()) -> pid().
create(Size) ->
  Pid = case start_link() of
          {ok, P} -> P;
          {error, {already_started, P}} -> P
        end,
  gen_server:call(Pid, {init, Size}),
  Pid.

-spec size(pid()) -> integer().
size(Pid) ->
  gen_server:call(Pid, size).

-spec read(pid()) -> {ok, term()} | {error, empty}.
read(Pid) ->
  gen_server:call(Pid, read).

-spec write(pid(), term()) -> ok.
write(Pid, Value) ->
  gen_server:call(Pid, {write, Value}).

-spec write_attempt(pid(), term()) -> ok | {error, full}.
write_attempt(Pid, Value) ->
  gen_server:call(Pid, {write_attempt, Value}).

%%% Internal functions

-spec bump_index(integer(), array:array(term())) -> integer().
bump_index(Index, Buffer) ->
  (Index + 1) rem array:size(Buffer).

-spec buffer_read(#state{}) -> {{ok, term()}, #state{}} | {{error, empty}, #state{}}.
buffer_read(State) ->
  Index = State#state.read_pos,
  case array:get(Index, State#state.buffer) of
    undefined -> {{error, empty} , State};
    Value -> Buffer = array:reset(Index, State#state.buffer),
             { {ok, Value}
             , State#state{ read_pos = bump_index(Index, Buffer)
                          , buffer = Buffer }
             }
  end.

-spec buffer_write(term(), #state{}) -> {ok, #state{}}.
buffer_write(Value, State) ->
  Index = State#state.write_pos,
  Buffer = array:set(Index, Value, State#state.buffer),
  {ok, State#state{ write_pos = bump_index(Index, Buffer)
                  , buffer = Buffer }}.
