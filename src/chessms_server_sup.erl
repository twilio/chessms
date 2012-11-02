%%%-------------------------------------------------------------------
%%% @author chadrs
%%% @copyright (C) 2012, chadrs
%%% @doc
%%%
%%% @end
%%% Created : 2012-05-22 17:55:32.791077
%%%-------------------------------------------------------------------
-module(chessms_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
        supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Players) ->
    supervisor:start_child(?SERVER, Players).

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
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    GameSup = {chessms_game_sup, {chessms_game_sup, start_link, []},
                       transient, brutal_kill, worker, [chessms_game_sup]},
    EngineSup = {chessms_engine_pool, {chessms_engine_pool, start_link, []},
                 permanent, 5000, supervisor, [chessms_engine_pool]},

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, [GameSup, EngineSup]}}.
