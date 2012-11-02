%%%-------------------------------------------------------------------
%%% @author chadrs
%%% @copyright (C) 2012, chadrs
%%% @doc
%%%
%%% @end
%%% Created : 2012-05-23 10:50:26.467234
%%%-------------------------------------------------------------------
-module(chessms_store).


%% gen_server callbacks
-export([init/0, get_game/1, add_game/3, delete_game/1, update_player_pref/3, get_player_prefs/1, log_move/4, get_game_log/1]).

-record(player_to_pid, {player, pid, game_id}).
-record(player_prefs, {player, preflist=[]}).
-record(move_log, {game_id, player, moveno, move}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Connect to mnesia database for chess data!
%%
%% @end
%%--------------------------------------------------------------------
init() ->
    mnesia:start(),
    mnesia:create_table(player_to_pid, [
        {attributes, record_info(fields, player_to_pid)}
    ]),
    mnesia:create_table(player_prefs, [
        {attributes, record_info(fields, player_prefs)}
    ]),
    mnesia:create_table(move_log, [
        {attributes, record_info(fields, move_log)}
    ]).

%%--------------------------------------------------------------------
%% @doc
%% Fetches the chessgame by Player (phone number or other id)
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
get_game(Player) ->
    case mnesia:dirty_read(player_to_pid, Player) of
        [#player_to_pid{pid=Pid}] ->
            case is_pid_alive(Pid) of
                true -> {ok, Pid};
                false -> {error, nogame}
            end;
        [] ->
            {error, nogame}
    end.

add_game(Players, Pid, GameId) ->
    Trans = fun() ->
        lists:map(fun(Player) -> mnesia:write(#player_to_pid{player=Player, pid=Pid, game_id=GameId}) end, Players)
    end,
    mnesia:transaction(Trans),
    {ok, Pid}.

delete_game(Player) ->
    case mnesia:dirty_read(player_to_pid, Player) of
        [#player_to_pid{} = Record] ->
            mnesia:dirty_delete_object(Record);
        _ ->
            ok
    end.

get_player_prefs(Player) ->
    Results = case mnesia:transaction(fun() -> mnesia:read(player_prefs, Player) end) of
        {atomic, []} ->
            [];
        {atomic, [#player_prefs{preflist=Options}]} ->
            Options
    end,
    Results.


update_player_pref(Player, PrefName, PrefValue) ->
    mnesia:transaction( fun() ->
        Prefs = get_player_prefs(Player),
        NewPrefs = proplists:delete(PrefName, Prefs) ++ [{PrefName, PrefValue}],
        mnesia:write(#player_prefs{player=Player, preflist=NewPrefs})
    end).

log_move(Game, Player, MoveNo, Move) ->
    mnesia:dirty_write(#move_log{game_id=Game, player=Player, moveno=MoveNo, move=Move}).

get_game_log(Game) ->
    mnesia:dirty_read(move_log, Game).

%%%===================================================================
%%% Internal functions
%%%===================================================================

is_pid_alive(Pid) when node(Pid) =:= node() ->
    is_process_alive(Pid);
is_pid_alive(Pid) ->
    case lists:member(node(Pid), nodes()) of
        false ->
            false;
        true ->
            case rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
                true ->
                    true;
                false ->
                    false;
                {badrpc, _Reason} ->
                    false
            end
    end.
