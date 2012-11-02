%%%-------------------------------------------------------------------
%%% @author chadrs
%%% @copyright (C) 2012, chadrs
%%% @doc
%%%
%%% @end
%%% Created : 2012-05-28 21:14:23.424328
%%%-------------------------------------------------------------------
-module(chessms_engine_pool).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([best_move/2, validate_move/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

best_move(Fen, Options) ->
    poolboy:transaction(engine_pool, fun(Worker) ->
        ok = gen_server:call(Worker, {uci, position, Fen}),
        gen_server:call(Worker, {uci, go, Options})
    end).

validate_move(Fen, Move) ->
    % hack to test if a move is valid using UCI.
    % Thanks to Marco Costalba for the idea.
    case best_move(Fen, [{depth, 0}, {searchmoves, Move}]) of
        {ok, "0000"} -> false;
        {ok, "(none)"} -> false;
        {ok, _} -> true
    end.


%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, Pools} = application:get_env(chessms_server, pools),


    % taken from poolboy example
    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
        PoolArgs = [{name, {local, Name}},
                    {worker_module, uci_worker}] ++ SizeArgs,
        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end, Pools),

    {ok, {SupFlags, PoolSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
