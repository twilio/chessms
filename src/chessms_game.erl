%%%-------------------------------------------------------------------
%%% @author chadrs
%%% @copyright (C) 2012, chadrs
%%% @doc
%%%
%%% @end
%%% Created : 2012-05-23 14:27:20.156472
%%%-------------------------------------------------------------------
-module(chessms_game).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-include("chessboard.hrl").

-record(state, {id, gamestate, white, black}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Players) ->
    gen_server:start_link(?MODULE, [Players], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Players) ->
    {White, Black} = case Players of
        [W] -> {W, computer};
        [W,B] -> {W, B}
    end,
    {ok, #state{white=White, black=Black, gamestate=chessboard:board()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({make_move, Player, Move}, _From, State=#state{gamestate=Game}) ->
    Valid = fun (G) -> chessms_engine_pool:validate_move(chessboard:board_to_fen(G), Move) end,
    % this clause will need refactoring sooner than later
    case Valid(Game) of
        true ->
            do_move(Player, chessboard:make_move(Move, Game), State);
        false ->
            {reply, {error, invalid_move, "Invalid move"}, State}
    end;
handle_call(get_board, _From, State=#state{gamestate=Game}) ->
    Reply = {ok, Game},
    {reply, Reply, State};
handle_call({set_fen, Player, FEN}, _From, State=#state{}) ->
    case set_state_for_board(chessboard:fen_to_board(FEN), Player) of
        NewState=#state{} ->
            {reply, ok, NewState};
        Error ->
            {reply, Error, State}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_move(_Player, NewBoard=#chessboard{}, State=#state{white=_Player, black=computer}) ->
    engine_move(State#state{gamestate=NewBoard});
do_move(_Player, NewBoard=#chessboard{}, State=#state{white=computer, black=_Player}) ->
    engine_move(State#state{gamestate=NewBoard});
do_move(_Player, NewBoard=#chessboard{},  State=#state{}) ->
    % playing another person; just return updated board and wait for other player to move
    {reply, {ok, valid_move}, State#state{gamestate=NewBoard}};
do_move(_, Error, State) ->
    % catchall for when make_move didn't return a chessboard
    {reply, Error, State}.

engine_move(State=#state{gamestate=Game}) ->
    BoardFen = chessboard:board_to_fen(Game),
    {ok, ComputerMove} = chessms_engine_pool:best_move(BoardFen, [{movetime, 500}]),
    case chessboard:make_move(ComputerMove, Game) of
        RepliedBoard=#chessboard{} ->
            {reply, {ok, valid_move}, State#state{gamestate=RepliedBoard}};
        Error ->
            {stop, "game over", {error, Error}, State}
    end.

set_state_for_board(Board=#chessboard{active=white}, Player) ->
    #state{gamestate=Board, white=Player, black=computer};
set_state_for_board(Board=#chessboard{active=black}, Player) ->
    #state{gamestate=Board, white=computer, black=Player};
set_state_for_board(Error, _) -> Error.
