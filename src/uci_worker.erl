%%%-------------------------------------------------------------------
%%% @author chadrs
%%% @copyright (C) 2012, chadrs
%%% @doc
%%%
%%% @end
%%% Created : 2012-04-28 09:36:34.505070
%%%-------------------------------------------------------------------
-module(uci_worker).

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

% for debugging
-export([build_go_option_line/1]).

-define(SERVER, ?MODULE).

-record(state, {engine, options=[], header}).

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
start_link(Args) ->
        gen_server:start_link(?MODULE, Args, []).



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
init(Args) ->
    ExecName = proplists:get_value(engine_path, Args, "./stockfish"),
    Engine = open_port({spawn, ExecName}, [{line, 1024}, use_stdio]),
    case initialize_uci(Engine) of
        {_Header, timeout} ->
            {stop, "Bad engine initialization"};
        {Header, Options} ->
            {ok, #state{engine=Engine, options=Options, header=Header}}
    end.

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
handle_call(options, _From, State) ->
    {reply, State#state.options, State};

handle_call({uci, isready}, _From, State) ->
    uci_cmd(State#state.engine, "isready"),
    Engine = State#state.engine,
    receive
        {Engine, {data, {eol, "readyok"}}} ->
            {reply, readyok, State}
    after 10000 ->
            {stop, "engine timeout", timeout, State}
    end;

handle_call({uci, setoption, [Name]}, _From, State) ->
    uci_cmd(State#state.engine, "setoption name " ++ Name),
    {reply, ok, State};

handle_call({uci, setoption, [Name, Value]}, _From, State) ->
    uci_cmd(State#state.engine, "setoption name " ++ Name ++ " value " ++ Value),
    {reply, ok, State};

handle_call({uci, ucinewgame}, _From, State) ->
    uci_cmd(State#state.engine, "ucinewgame"),
    {reply, ok, State};

handle_call({uci, position, startfen}, _From, State) ->
    uci_cmd(State#state.engine, "position startfen"),
    {reply, ok, State};

handle_call({uci, position, Fen}, _From, State) ->
    uci_cmd(State#state.engine, "position fen " ++ Fen),
    {reply, ok, State};

handle_call({uci, position, Fen, Moves}, _From, State) ->
    MovesStr = string:join(Moves, " "),
    uci_cmd(State#state.engine, "position " ++ Fen ++ " moves " ++ MovesStr),
    {reply, ok, State};

handle_call({uci, go, Options}, _From, State=#state{engine=Engine}) ->
    GoStr = build_go_option_line(Options),
    uci_cmd(Engine, GoStr),
    receive
        {Engine, {data, {eol, "bestmove " ++ MoveInfo}}} ->
            case string:tokens(MoveInfo, " ") of
                [Move, "ponder", _Ponder] ->
                    % maybe we should return the ponder??
                    {reply, {ok, Move}, State};
                [Move] ->
                    {reply, {ok, Move}, State};
                _Other ->
                    {reply, {error, MoveInfo}, State}
            end
    after 20000 ->
        {stop, engine_unresponsive, State}
    end;

handle_call({uci, stop}, _From, State) ->
    {reply, ok, State};

handle_call({uci, ponderhit}, _From, State) ->
    {reply, ok, State};

handle_call({uci, quit}, _From, State) ->
    {stop, ok, State};

handle_call(_Other, _From, State) ->
    {noreply, State}.

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
handle_info(timeout, State) ->
    {stop, "Timeout", State};

handle_info(Info, State) ->
    io:format("info: ~p", [Info]),
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

initialize_uci(Port) ->
    Header = receive
        {Port, {data,{eol, Data}}} ->
            Data
    after 1000 ->
        none
    end,
    uci_cmd(Port, "uci"),
    Options = options_loop(Port, []),
    {Header, Options}.

uci_cmd(Port, Command) ->
    Port ! {self(), {command, Command ++ "\n"}}.

options_loop(Port, Accum) ->
    receive
        {Port, {data, {eol, "uciok"}}} ->
            Accum;
        {Port, {data, {eol, Option}}} ->
            options_loop(Port, [Option | Accum])
    after 2000 ->
            timeout
    end.

build_go_option_line(Options) ->
    PossibleOptions = [searchmoves, ponder, wtime, btime,
                       winc, binc, movestogo, depth, nodes,
                       mate, movetime, infinite],
    lists:foldl(fun(Option, Accum) ->
        case {Option, proplists:get_value(Option, Options)} of
            {_, undefined} ->
                Accum;
            {infinite, _Val} ->
               Accum ++  " infinite";
            {ponder, _Val} ->
               Accum ++  " infinite";
            {Option, Value} when is_list(Value) ->
                Accum ++ io_lib:format("~p ~s ", [Option, Value]);
            {Option, Value} ->
                Accum ++ io_lib:format("~p ~p ", [Option, Value])
        end
    end, "go ", PossibleOptions).
