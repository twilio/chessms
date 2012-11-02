-module(chessms_handler).
-export([init/3, handle/2, terminate/2]).

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, PostData, _} = cowboy_req:body_qs(Req),
    Input = string_param(<<"Body">>, PostData, fun string:to_lower/1),
    Player = {phone, string_param(<<"From">>, PostData)},
    {ok, Req2} = case Input of
        undefined ->
            cowboy_req:reply(400, [], "Bad request.", Req);
        "help" ->
            do_help(Req);
        "play" ->
            do_play(Req, Player);
        "set " ++ Setting ->
            do_setting(Req, Setting, Player);
        "showfen " ++ Fen ->
            do_showfen(Req, Player, Fen);
        "setfen " ++ Fen ->
            do_setfen(Req, Player, Fen);
        "board" ->
            do_board(Req);
        "flip" ->
            do_flip_board(Req, Player);
        Move when length(Input) == 4 orelse length(Input) == 5 ->
            do_move(Req, Player, Move);
        _ ->
            do_help(Req)
    end,
    {ok, Req2, State}.

string_param(Key, PostData) ->
    string_param(Key, PostData, fun (X) -> X end).

string_param(Key, PostData, Filter) ->
    case proplists:get_value(Key, PostData) of
        undefined ->
            undefined;
        Other when is_binary(Other) ->
            Filter(binary_to_list(Other));
        Else ->
            Filter(Else)
    end.

chessboard_twiml(Move, Player) ->
    GPid = case chessms_store:get_game(Player) of
        {ok, Pid} ->
            Pid;
        {error, nogame} ->
            {ok, Pid} = chessms_game_sup:start_child([Player]),
            chessms_store:add_game([Player], Pid, undefined),
            Pid
    end,
    case gen_server:call(GPid, {make_move, Player, Move}) of
        {ok, valid_move} ->
            format_board_for_player(Player, GPid, white);
        {error, invalid_move, Reason} ->
            "Error: " ++ Reason;
        {error, null_move} ->
            "Error: Invalid move"
    end.

format_board_for_player(Player, Pid, Color)  when is_pid(Pid) ->
    {ok, Board} = gen_server:call(Pid, get_board),
    format_board_for_player(Player, Board, Color);
format_board_for_player(Player, Game, Color) ->
    Prefs = chessms_store:get_player_prefs(Player),
    {PieceCB, SquareCB, JoinCB} = get_style_callbacks(proplists:get_value("style", Prefs, unicode)),
    Board = chessboard:render_board(Game, Color, PieceCB, SquareCB, JoinCB),
    unicode:characters_to_binary(Board, unicode, utf8).

get_style_callbacks("unicode") -> {
        fun chessboard:unicode_piece/1,
        fun chessboard:unicode_square/1,
        fun (Lines) -> strip_trailing_white_squares(Lines, chessboard:unicode_square(light)) end
    };
get_style_callbacks("ascii") -> {
        fun chessboard:ascii_piece/1,
        fun chessboard:ascii_square/1,
        fun (Lines) -> string:join(Lines, "\n") end
    };
get_style_callbacks(_) -> get_style_callbacks("unicode").

strip_trailing_white_squares(Ranks, SquareCharacter) ->
    % lame this is needed, erlang's string:strip is not
    % unicode whitespace characters aware like Python's is.
    % Another lame reason it's required is that a chessboard
    % is more than 140 bytes (142) which is the limit for SMS.
    Stripped = lists:map(fun(S) -> string:strip(S, right, SquareCharacter) end, Ranks),
    string:join(Stripped, "\n").

do_flip_board(Req, Player) ->
    % TODO: find their color
    Response = case chessms_store:get_game(Player) of
        {ok, Pid} ->
            format_board_for_player(Player, Pid, black);
        {error, nogame} ->
            "Error: no active board found to flip"
    end,
    twiml_cowboy_response(Req, Response).

twimlsms(Texts) ->
    % Texts is a list of messages that should each be surrounded by <Sms></Sms>
    Children = [{'Sms', [], [[Text]]} || Text <- Texts],
    xmerl:export_simple([{'Response', [], Children}], xmerl_xml).

% twiml_cowboy_response works when the 2nd arg is a string or a list of strings
twiml_cowboy_response(Req, [First|_Rest]=Messages) when is_list(First) ->
    Twiml = twimlsms(Messages),
    cowboy_req:reply(200, [{<<"Content-Type">>, <<"text/xml; charset=utf-8">>}], Twiml, Req);

twiml_cowboy_response(Req, Resp) ->
    twiml_cowboy_response(Req, [Resp]).


do_setting(Req, Setting, Player) ->
    % TODO: integrate with datastore
    Resp = case string:tokens(Setting, " ") of
        [N, V] ->
            case chessms_store:update_player_pref(Player, N, V) of
                {atomic, ok} ->
                    "Set " ++ N ++ " to " ++ V;
                _Err ->
                    "Failed to set setting"
            end;
        _Other ->
            "Usage: set `settingname` [option]"
    end,
    twiml_cowboy_response(Req, Resp).

do_showfen(Req, Player, Fen) ->
    case chessboard:fen_to_board(Fen) of
        {error, invalid_fen} ->
            twiml_cowboy_response(Req, "Invalid FEN");
        Board ->
            twiml_cowboy_response(Req, format_board_for_player(Player, Board, white))
    end.

do_setfen(Req, Player, Fen) ->
    GPid = case chessms_store:get_game(Player) of
        {ok, Pid} ->
            Pid;
        {error, nogame} ->
            {ok, Pid} = chessms_game_sup:start_child([Player]),
            chessms_store:add_game([Player], Pid, undefined),
            Pid
    end,
    Resp = case gen_server:call(GPid, {set_fen, Player, Fen}) of
        ok ->
            format_board_for_player(Player, GPid, white);
        {error, invalid_fen} ->
            "Error: invalid FEN"
    end,
    twiml_cowboy_response(Req, Resp).



do_board(Req) ->
    Key = "a b c d e f g h
8
7
6
5
4
3
2
1",
    %TODO: know if they're playing white or black
    twiml_cowboy_response(Req, Key).

do_help(Req) ->
    Helptext = "Welcome to chessms. To start a game type 'play' Commands: help, board, flip",
    twiml_cowboy_response(Req, Helptext).

do_play(Req, Player) ->
    Playtext = "New game started! You're playing white. Make a move (e.g. e2e4)",
    % delete any existing games
    chessms_store:delete_game([Player]),
    % start a new game
    {ok, Pid} = chessms_game_sup:start_child([Player]),
    chessms_store:add_game([Player], Pid, undefined),
    Board = format_board_for_player(Player, Pid, white),
    twiml_cowboy_response(Req, [Playtext, Board]).

do_move(Req, Player, Move) ->
    twiml_cowboy_response(Req, chessboard_twiml(Move, Player)).

terminate(_Req, _State) ->
    ok.


