-module(bank_account).
-behaviour(gen_server).

%% Exercism API
-export([create/0, balance/1, deposit/2, withdraw/2, charge/2, close/1]).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, { account
               , balance = 0
               }).

%%% API

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% gen_server callbacks

init([]) -> {ok, #state{}}.

handle_call(create, _From, _State) ->
  AccountID = crypto:rand_bytes(16),
  {reply, {ok, AccountID}, #state{account = AccountID}};
handle_call({close, AccountID}, _From, State) ->
  {Reply, NewState} = case State#state.account of
            AccountID -> {State#state.balance, #state{}};
            _ -> {0, State}
          end,
  {reply, Reply, NewState};
handle_call({balance, AccountID}, _From, State) ->
  Reply = case State#state.account of
            AccountID -> State#state.balance;
            _ -> {error, account_closed}
          end,
  {reply, Reply, State};
handle_call({adjust, _AccountID, Amount}, _From, State) ->
  Balance = State#state.balance + Amount,
  {Reply, NewState} = case Balance >= 0 of
                        true -> {{ok, Amount}, State#state{balance = Balance}};
                        _ -> {{ok, Amount - Balance}, State#state{balance = 0}}
                      end,
  {reply, Reply, NewState};
handle_call({charge, _AccountID, Amount}, _From, State) ->
  Balance = State#state.balance + Amount,
  {Reply, NewState} = case Balance >= 0 of
                        true -> {{ok, Amount}, State#state{balance = Balance}};
                        _ -> {error, State}
                      end,
  {reply, Reply, NewState};
handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%% Exercism API

create() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
  {ok, AccountID} = gen_server:call(?SERVER, create),
  AccountID.

balance(AccountID) ->
  gen_server:call(?SERVER, {balance, AccountID}).

deposit(_AccountID, Amount) when Amount =< 0 ->
  0;
deposit(AccountID, Amount) ->
  case gen_server:call(?SERVER, {adjust, AccountID, Amount}) of
    {ok, Amount} -> Amount;
    error -> error
  end.

withdraw(_AccountID, Amount) when Amount =< 0 ->
  0;
withdraw(AccountID, Amount) ->
  case gen_server:call(?SERVER, {adjust, AccountID, -Amount}) of
    {ok, Withdrawal} -> -Withdrawal;
    error -> error
  end.

charge(_AccountID, Charge) when Charge =< 0 ->
  0;
charge(AccountID, Charge) ->
  case gen_server:call(?SERVER, {charge, AccountID, -Charge}) of
    {ok, _} -> Charge;
    error -> 0
  end.

close(AccountID) ->
  gen_server:call(?SERVER, {close, AccountID}).
