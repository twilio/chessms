%%%-------------------------------------------------------------------
%%% @author chadrs
%%% @copyright (C) 2012, chadrs
%%% @doc
%%%
%%% @end
%%% Created : 2012-05-21 14:35:48.237300
%%%-------------------------------------------------------------------
-module(chessboard).
% board functions
-export([board/0, board_to_fen/1, fen_to_board/1, print_board/1, render_board/2, render_board_ascii/2, render_board/5]).
% moves functions
-export([make_move/2, short_notation/2, expanded_notation/2, play/0]).
-export([unicode_piece/1, unicode_square/1, ascii_piece/1, ascii_square/1]).

-include("chessboard.hrl").

board() -> fen_to_board(?STARTPOS_FEN).

play() ->
    play(board()).

play(Board) ->
    io:format("~n~ts~n", [render_board(Board, white)]),
    io:format("board fen is ~s~n", [board_to_fen(Board)]),
    {ok, [Move]} = io:fread("white: ", "~s"),
    Board2 = make_move(Move, Board),
    io:format("~n~ts~n", [render_board(Board2, black)]),
    io:format("board fen is ~s~n", [board_to_fen(Board2)]),
    {ok, [Move2]} = io:fread("black: ", "~s"),
    play(make_move(Move2, Board2)).


board_to_fen(Board=#chessboard{}) ->
    string:join([
        fen_part(placements, Board#chessboard.placements),
        fen_part(active, Board#chessboard.active),
        fen_part(castling, Board#chessboard.castling),
        fen_part(enpassant, Board#chessboard.enpassant),
        fen_part(number, Board#chessboard.halfmove_clock),
        fen_part(number, Board#chessboard.fullmove_number)
    ], " ").

fen_to_board(Fen) ->
    % 1. Parse fen.
    Parts = string:tokens(Fen, " "),
    fen_parts_to_board(Parts).

print_board(CB=#chessboard{}) ->
    io:format(render_board(CB, white)),
    io:format("~n"),
    io:format(render_board(CB, black)).

render_board(Board, PointOfView) ->
    render_board(Board, PointOfView, fun unicode_piece/1,
                  fun unicode_square/1, fun (Lines) -> string:join(Lines, "\n") end).

render_board_ascii(Board, PointOfView) ->
    render_board(Board, PointOfView, fun ascii_piece/1,
                  fun ascii_square/1, fun (Lines) -> string:join(Lines, "\n") end).

render_board(#chessboard{placements=Board}, PointOfView, LookupPiece, LookupColor, JoinFunc) ->
    GetSquare = fun
        ({empty, Index}) -> LookupColor(color_of_square(Index));
        ({Piece, _}) -> LookupPiece(Piece)
    end,
    BoardList = tuple_to_list(Board),
    Indexes = lists:seq(0, length(BoardList) - 1),
    PiecesList = lists:map(GetSquare, lists:zip(BoardList, Indexes)),
    Ranks = case PointOfView of
        white ->
            % print like this:
            % 0 1 3 4 ...
            % 8 9 10 11 ...
            lists:reverse(octets(PiecesList));
        black ->
            % print like this
            % 63 62 61 ...
            % 55 54 53 ...
            lists:map(fun lists:reverse/1, octets(PiecesList))
    end,
    JoinFunc(Ranks).

short_notation(_Board, _MoveStr) ->
    %%% XXX: Implement me
    #chessmove{}.

-spec expanded_notation(iolist(), #chessboard{}) -> #chessmove{} | {'error', 'invalid_move', iolist()}.
expanded_notation(MoveStr, #chessboard{placements=Placements, active=Active, enpassant=EnPassantSquare}) ->
    % XXX: factor out validation so it can be used in all the places I do the same thing.
    {From, To, Promotion} = case string:to_lower(MoveStr) of
        "(none)" ->
            {undefined, undefined, undefined};
        "0000" ->
            {undefined, undefined, undefined};
        [FFile, FRank, $x, TFile, TRank] ->
            {name_to_square([FFile, FRank]), name_to_square([TFile, TRank]), undefined};
        [FFile, FRank, $x, TFile, TRank, Promote] ->
            {_Color, PromotePiece} = fen_to_piece(Promote),
            {name_to_square([FFile, FRank]), name_to_square([TFile, TRank]), PromotePiece};
        [FFile, FRank, TFile, TRank] ->
            {name_to_square([FFile, FRank]), name_to_square([TFile, TRank]), undefined};
        [FFile, FRank, TFile, TRank, Promote] ->
            {_Color, PromotePiece} = fen_to_piece(Promote),
            {name_to_square([FFile, FRank]), name_to_square([TFile, TRank]), PromotePiece};
        _Other ->
            {undefined, undefined, undefined}
    end,
    case {From, To} of
        {undefined, undefined} ->
            {error, null_move};
        {ToSquareNo, FromSquareNo} when is_integer(ToSquareNo)  andalso is_integer(FromSquareNo) ->
            case element(From, Placements) of
                {Active, Piece} ->
                    Move = #chessmove{
                        to=To,
                        from=From,
                        promotion=Promotion,
                        chesspiece=Piece,
                        special=special_kind({Active, Piece}, From, To, Promotion, EnPassantSquare),
                        side=Active
                    },
                    case element(To, Placements) of
                        {Active, _KindOfPiece} ->
                            % can't move on top of your own piece
                            {error, invalid_move, "Can't capture your own piece"};
                        empty when Move#chessmove.special == enpassant ->
                            Move#chessmove{capture=true};
                        empty ->
                            Move;
                        {_, _KindOfPiece} ->
                            Move#chessmove{capture=true}
                    end;
                empty ->
                    {error, invalid_move, "No piece to move on that square."};
                {_OtherColor, _Piece} ->
                    {error, invalid_move, "Can't move a piece that isn't yours!"}
            end;
        {invalid_square, _} ->
            {error, invalid_move, "Invalid `from` square"};
        {_, invalid_square} ->
            {error, invalid_move, "Invalid `to` square"};
        _OtherMove ->
            {error, invalid_move, "Bad square input"}
    end.

make_move(Move=#chessmove{chesspiece=PieceType, from=From, side=Color},
        Board=#chessboard{placements=Placements}) ->
    % make sure there's at least a piece to move.
    case element(From, Placements) of
        {Color, PieceType} ->
            update_board_for_move(Move, Board);
        empty ->
            {error, invalid_move, "Can't move from an empty square"};
        _ ->
            {error, invalid_move, "Not your piece"}
    end;

make_move(MoveStr, Board) when is_list(MoveStr) ->
    ParsedMoved = expanded_notation(MoveStr, Board),
    case ParsedMoved of
        #chessmove{} ->
            make_move(ParsedMoved, Board);
        Other ->
            Other
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

square_file_index(Square) -> (Square - 1) rem 8.

square_rank_index(Square) -> (Square - 1) div 8.

square_to_name(Index) when is_integer(Index) ->
    Rank = square_rank_index(Index) + $1,
    File = square_file_index(Index) + $a,
    [File, Rank].

name_to_square([File, Rank]) when File  >= $a andalso File =< $h andalso Rank >= $1 andalso Rank =< $8 ->
    (Rank - $1) * 8 + (File - $a) + 1;
name_to_square([File, Rank]) when File  >= $A andalso File =< $H ->
    name_to_square(string:to_lower([File, Rank]));
name_to_square(_) ->
    invalid_square.

color_of_square(Index) when ((Index rem 8) rem 2) == ((Index  div 8) rem 2) -> dark;
color_of_square(Index) when is_integer(Index) -> light.

calc_halfmove_clock(_, #chessmove{chesspiece=pawn}) -> 0;
calc_halfmove_clock(_, #chessmove{capture=true}) -> 0;
calc_halfmove_clock(Clock, _) -> Clock + 1.

fen_parts_to_board([Placement, Active, Castling, EnPassant, HalfMoves, Fullmoves]) ->
    Places = parse_fen_placements(Placement),
    #chessboard{
        % 2. set placement board
        placements=Places,
        % 3. Set the easy stuff
        active=fen_part(active, Active),
        castling=fen_part(castling, Castling),
        enpassant=fen_part(enpassant, EnPassant),
        halfmove_clock=list_to_integer(HalfMoves),
        fullmove_number=list_to_integer(Fullmoves)
    };

fen_parts_to_board(_Invalid) ->
    {error, invalid_fen}.

parse_fen_placements(String) ->
    % XXX: Also generate bitboards?
    Ranks = string:tokens(String, "/"),
    ParsedRanks = lists:map(fun parse_fen_rank/1, Ranks),
    list_to_tuple(lists:flatten(lists:reverse(ParsedRanks))).

parse_fen_rank(RankString) ->
    lists:map(fun fen_to_piece/1, RankString).

update_board_for_move(Move=#chessmove{to=To, from=From, side=Color, special=MoveType},
                      Board=#chessboard{placements=Placements}) ->
    % no validation, just makes the move!
    {MoveNo, Turn} = case Color of
        black ->
            {Board#chessboard.fullmove_number + 1, white};
        white ->
            {Board#chessboard.fullmove_number, black}
    end,
    % move it in the placements tuple
    %Captured = element(To, Board#chessboard.placements),
    UpdatedPlacements = case MoveType of
        normal ->
            update_placements_for_move(From, To, Placements);
        castle ->
            KingMoved = update_placements_for_move(From, To, Placements),
            {RookFrom, RookTo} = rook_moves_for_castle(From, To),
            update_placements_for_move(RookFrom, RookTo, KingMoved);
        enpassant ->
            PawnMoved = update_placements_for_move(From, To, Placements),
            CapturedPawnSquare = enpassant_captured_pawn_square(From, To),
            setelement(CapturedPawnSquare, PawnMoved, empty);
        promotion when Move#chessmove.promotion =/= undefined ->
            Pickup = setelement(From, Placements, empty),
            setelement(To, Pickup, {Color, Move#chessmove.promotion});
        promotion ->
            Pickup = setelement(From, Placements, empty),
            setelement(To, Pickup, {Color, queen})
    end,
    HMC = calc_halfmove_clock(Board#chessboard.halfmove_clock, Move),
    EnPassant = detect_enpassant(Move),
    Castling = update_castling(Move, Board#chessboard.castling),

    Board#chessboard{
        placements=UpdatedPlacements, fullmove_number=MoveNo, halfmove_clock=HMC,
        active=Turn, enpassant=EnPassant, castling=Castling}.

update_placements_for_move(From, To, Placements) ->
    Piece = element(From, Placements),
    Pickup = setelement(From, Placements, empty),
    setelement(To, Pickup, Piece).


fen_part(placements, Board)
        when is_tuple(Board)->
    % "hard" part
    Ranks = lists:reverse(octets(tuple_to_list(Board))),
    FenLines = lists:map(fun fen_line_from_rank/1, Ranks),
    string:join(FenLines, "/");
fen_part(active, white) -> "w";
fen_part(active, black) -> "b";
fen_part(active, "w") -> white;
fen_part(active, "b") -> black;
fen_part(castling, CastleStr) when is_list(CastleStr) ->
    InitialStatus = #castling_rights{white_kingside=false,
                                    white_queenside=false,
                                    black_kingside=false,
                                    black_queenside=false},
    Parse = fun
        (_, "-", Status) -> Status;
        (_, "", Status) -> Status;
        (Parse, [Head|Tail], Status) ->
            NewStatus = case Head of
                $K -> Status#castling_rights{white_kingside=true};
                $Q -> Status#castling_rights{white_queenside=true};
                $k -> Status#castling_rights{black_kingside=true};
                $q -> Status#castling_rights{black_queenside=true}
            end,
            Parse(Parse, Tail, NewStatus)
    end,
    Parse(Parse, CastleStr, InitialStatus);
fen_part(castling, CastleStr) when is_tuple(CastleStr) ->
    Symbols = [{CastleStr#castling_rights.white_kingside, $K},
               {CastleStr#castling_rights.white_queenside, $Q},
               {CastleStr#castling_rights.black_kingside, $k},
               {CastleStr#castling_rights.black_queenside, $q}],
    lists:foldr(fun
            ({true, Val}, Acum) -> [Val|Acum];
            ({false, _}, Acum) -> Acum
        end, "", Symbols);
fen_part(number, Number) -> integer_to_list(Number);
fen_part(enpassant, none) -> "-";
fen_part(enpassant, "-") -> none;
fen_part(enpassant, Square) when is_integer(Square) -> square_to_name(Square);
fen_part(enpassant, Square) when is_list(Square) -> name_to_square(Square);
fen_part(_Other, Arg) ->
    %% Assume if we get here we can just iofmt to a string
    io_lib:format("~p", [Arg]).

fen_line_from_rank(Rank) ->
    rev_fen_line_from_rank(Rank, 0).

rev_fen_line_from_rank([], 0) -> [];
rev_fen_line_from_rank([], Empties) -> integer_to_list(Empties);
rev_fen_line_from_rank([Square|Rank], 0) ->
    case fen_piece(Square) of
        undefined ->
            rev_fen_line_from_rank(Rank, 1);
        FenChar ->
            [FenChar|rev_fen_line_from_rank(Rank, 0)]
    end;
rev_fen_line_from_rank([Square|Rank], Empties) ->
    case fen_piece(Square) of
        undefined ->
            rev_fen_line_from_rank(Rank, Empties + 1);
        FenChar ->
            [$0 + Empties, FenChar|rev_fen_line_from_rank(Rank, 0)]
    end.

unicode_piece({white, king}) -> 16#2654;
unicode_piece({white, queen}) -> 16#2655;
unicode_piece({white, rook}) -> 16#2656;
unicode_piece({white, bishop}) -> 16#2657;
unicode_piece({white, knight}) -> 16#2658;
unicode_piece({white, pawn}) -> 16#2659;
unicode_piece({black, king}) -> 16#265A;
unicode_piece({black, queen}) -> 16#265B;
unicode_piece({black, rook}) -> 16#265C;
unicode_piece({black, bishop}) -> 16#265D;
unicode_piece({black, knight}) -> 16#265E;
unicode_piece({black, pawn}) -> 16#265F;
unicode_piece(empty) -> undefined.

unicode_square(dark) -> 16#2593;
unicode_square(light) -> 16#2001.

ascii_square(dark) -> $.;
ascii_square(light) -> $_.  % space

ascii_piece(empty) -> undefined;
ascii_piece(Piece) -> fen_piece(Piece).

fen_piece({white, king}) -> $K;
fen_piece({white, queen}) -> $Q;
fen_piece({white, rook}) -> $R;
fen_piece({white, bishop}) -> $B;
fen_piece({white, knight}) -> $N;
fen_piece({white, pawn}) -> $P;
fen_piece({black, king}) -> $k;
fen_piece({black, queen}) -> $q;
fen_piece({black, rook}) -> $r;
fen_piece({black, bishop}) -> $b;
fen_piece({black, knight}) -> $n;
fen_piece({black, pawn}) -> $p;
fen_piece(empty) -> undefined.

fen_to_piece(Piece) ->
    case Piece of
        $K -> {white, king};
        $Q -> {white, queen};
        $R -> {white, rook};
        $B -> {white, bishop};
        $N -> {white, knight};
        $P -> {white, pawn};
        $k -> {black, king};
        $q -> {black, queen};
        $r -> {black, rook};
        $b -> {black, bishop};
        $n -> {black, knight};
        $p -> {black, pawn};
        Blanks -> lists:duplicate(Blanks - $0, empty)
    end.

special_kind({_, pawn}, _From, To, undefined, EnPassantSquare) when To == EnPassantSquare -> enpassant;
special_kind({_, pawn}, _, _, Promoted, _) when Promoted =/= undefined -> promotion;
% sadly these functions depends on having 1 indexed board :(
special_kind({white, king}, 5, 7, undefined, _) -> castle;
special_kind({white, king}, 5, 3, undefined, _) -> castle;
special_kind({black, king}, 61, 63, undefined, _) -> castle;
special_kind({black, king}, 61, 59, undefined, _) -> castle;
special_kind(_, _, _, undefined, _) -> normal.

detect_enpassant(#chessmove{chesspiece=pawn, from=From, to=To}) ->
    {FRank, TRank} = {square_rank_index(From), square_rank_index(To)},
    % if the piece moved two spaces, then the enpassant square is their average
    case abs(FRank - TRank) of
        2 ->
            (From + To) div 2;
        _Hopefully1 ->
            none
    end;
detect_enpassant(_) ->
    none.

% short circuit anyone who has no possible castling
update_castling(_, CS=#castling_rights{white_kingside=false, white_queenside=false, black_kingside=false, black_queenside=false}) ->
    CS;
% Reasons castling status would change:
% 1. Move your king (includes castling)
update_castling(#chessmove{chesspiece=king, side=white}, CS) ->
    CS#castling_rights{
        white_kingside=false,
        white_queenside=false
    };
update_castling(#chessmove{chesspiece=king, side=black}, CS) ->
    CS#castling_rights{
        black_kingside=false,
        black_queenside=false
    };
% 2. Move a rook for the first time
update_castling(#chessmove{chesspiece=rook, side=white, from=1}, S=#castling_rights{white_queenside=true}) ->
    S#castling_rights{white_queenside=false};
update_castling(#chessmove{chesspiece=rook, side=white, from=8}, S=#castling_rights{white_kingside=true}) ->
    S#castling_rights{white_kingside=false};
update_castling(#chessmove{chesspiece=rook, side=black, from=57}, S=#castling_rights{black_queenside=true}) ->
    S#castling_rights{black_queenside=false};
update_castling(#chessmove{chesspiece=rook, side=black, from=64}, S=#castling_rights{black_kingside=true}) ->
    S#castling_rights{black_kingside=false};
% 3. Have a rook captured
update_castling(#chessmove{capture=true, side=white, to=57}, S=#castling_rights{black_queenside=true}) ->
    S#castling_rights{black_queenside=false};
update_castling(#chessmove{capture=true, side=white, to=64}, S=#castling_rights{black_kingside=true}) ->
    S#castling_rights{black_kingside=false};
update_castling(#chessmove{capture=true, side=black, to=1}, S=#castling_rights{white_queenside=true}) ->
    S#castling_rights{white_queenside=false};
update_castling(#chessmove{capture=true, side=black, to=8}, S=#castling_rights{white_kingside=true}) ->
    S#castling_rights{white_kingside=false};
% I think that's it...
update_castling(_, CS) -> CS.

% probably could have use math to do this, but this seems easier.
rook_moves_for_castle(5, 7) -> {8, 6};
rook_moves_for_castle(5, 3) -> {1, 4};
rook_moves_for_castle(61, 63) -> {64, 62};
rook_moves_for_castle(61, 59) -> {57, 60}.

% captured square has the rank of the From and the file of the To
enpassant_captured_pawn_square(From, To) -> 8 * square_rank_index(From) + square_file_index(To) + 1.

octets([]) -> [];
octets(List) ->
    {Rank, Rest} = lists:split(8, List),
    [Rank] ++ octets(Rest).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

display_test() ->
    % not sure how to test the output of this, but
    % lets at least make sure it doesn't crash.
    ?assert(is_list(render_board(board(), white))),
    ?assert(is_list(render_board(board(), black))),
    ?assert(is_list(render_board_ascii(board(), black))),
    %print_board(board()).
    later.

move_parse_test() ->
    TestingBoard = fen_to_board("r4knr/2PQ2pp/5p2/1B6/4P3/P4N2/P1P2PPP/R3K2R w KQkq - 0 12"),
    ?assertEqual(
        #chessmove{to=name_to_square("a4"), from=name_to_square("a2"), promotion=undefined,
                   chesspiece=pawn, special=normal, side=white},
        expanded_notation("A2A4", TestingBoard)
    ),
    ?assertEqual(
        #chessmove{to=name_to_square("g7"), from=name_to_square("d7"), promotion=undefined,
                   chesspiece=queen, special=normal, side=white, capture=true},
        expanded_notation("d7g7", TestingBoard)
    ),
    ?assertEqual(expanded_notation("d7g7", TestingBoard), expanded_notation("d7xg7", TestingBoard)),
    ?assertMatch({error, invalid_move, _}, expanded_notation("i2j4", TestingBoard)),
    ?assertMatch({error, invalid_move, _}, expanded_notation("a2j4", TestingBoard)),
    ?assertMatch({error, invalid_move, _}, expanded_notation("d7c7", TestingBoard)),
    ?assertMatch({error, null_move}, expanded_notation("(none)", TestingBoard)),
    ?assertMatch({error, null_move}, expanded_notation("adsfas", TestingBoard)),
    ?assertMatch({error, null_move}, expanded_notation("0000", TestingBoard)),

    ok.

make_move_test() ->
    TestingBoard = fen_to_board("r4knr/2PQ2pp/5p2/1B6/4P3/P4N2/P1P2PPP/R3K2R w KQkq - 5 12"),
    % test haflmove clock with capture
    Capture = make_move("d7g7", TestingBoard),
    ?assertEqual(0, Capture#chessboard.halfmove_clock),
    % invalid moves
    ?assertMatch({error, null_move}, make_move("0000", TestingBoard)),
    ?assertMatch({error, invalid_move, _}, make_move("e2e4", TestingBoard)),
    ?assertMatch({error, invalid_move, _}, make_move("a8b8", TestingBoard)).


fen_test() ->
    ?assertEqual({error, invalid_fen}, fen_to_board("k6K/8/8/8/8/8/8/8 wutever")),
    ?assertEqual({error, invalid_fen}, fen_to_board("k6K/8/8/8/8/8 - - 20 40")),
    ?assertEqual(?STARTPOS_FEN, board_to_fen(fen_to_board(?STARTPOS_FEN))).

white_castling_test() ->
    TestingBoard = fen_to_board("r4knr/2PQ2pp/5p2/1B6/4P3/P4N2/P1P2PPP/R3K2R w KQkq - 0 12"),
    CastleQS = make_move("e1c1", TestingBoard),
    ?assertEqual("r4knr/2PQ2pp/5p2/1B6/4P3/P4N2/P1P2PPP/2KR3R b kq - 1 12", board_to_fen(CastleQS)),
    CastleKS = make_move("e1g1", TestingBoard),
    ?assertEqual("r4knr/2PQ2pp/5p2/1B6/4P3/P4N2/P1P2PPP/R4RK1 b kq - 1 12", board_to_fen(CastleKS)),

    % thing that take away castling rights.
    ?assertEqual(#castling_rights{
            white_queenside=true, white_kingside=true,
            black_queenside=true, black_kingside=true
            }, TestingBoard#chessboard.castling),
    KingMoved = make_move("e1d1", TestingBoard),
    ?assertEqual(#castling_rights{
            white_kingside=false, white_queenside=false,
            black_queenside=true, black_kingside=true
            }, KingMoved#chessboard.castling),
    ok.

black_castling_test() ->
    TestingBoard = fen_to_board("r3k2r/8/8/8/8/8/8/R3K2R b KQkq - 0 12"),
    CastleKS = make_move("e8g8", TestingBoard),
    ?assertEqual("r4rk1/8/8/8/8/8/8/R3K2R w KQ - 1 13", board_to_fen(CastleKS)),
    CastleQS = make_move("e8c8", TestingBoard),
    ?assertEqual("2kr3r/8/8/8/8/8/8/R3K2R w KQ - 1 13", board_to_fen(CastleQS)),

    KingMoved = make_move("e8d8", TestingBoard),
    ?assertEqual(#castling_rights{
            white_kingside=true, white_queenside=true,
            black_queenside=false, black_kingside=false
            }, KingMoved#chessboard.castling),

    % bad castle; board won't validate but shouldn't consider the move a "castle"
    CantCastle = fen_to_board("r3k2r/2PQ2pp/5p2/1B6/4P3/P4N2/P1P2PPP/R3K2R b - - 0 12"),
    ?assertMatch(#chessmove{special=normal}, expanded_notation("e8d8", CantCastle)),

    ok.

promotion_test() ->
    TestingBoard = fen_to_board("r4knr/2PQ2pp/5p2/1B6/4P3/P4N2/P1P2PPP/R3K2R w KQkq - 0 12"),
    Promoted = make_move("c7c8q", TestingBoard),
    ?assertEqual("r1Q2knr/3Q2pp/5p2/1B6/4P3/P4N2/P1P2PPP/R3K2R b KQkq - 0 12", board_to_fen(Promoted)).

en_passant_test() ->
    EnPassantPosition = fen_to_board("rnbqkbnr/p1pppppp/8/8/1pPP4/5N2/PP2PPPP/RNBQKB1R b KQkq c3 0 3"),

    ?assertMatch(
        #chessmove{side=black, special=enpassant, capture=true, chesspiece=pawn},
        expanded_notation("b4c3", EnPassantPosition)),

    ?assertEqual("rnbqkbnr/p1pppppp/8/8/3P4/2p2N2/PP2PPPP/RNBQKB1R w KQkq - 0 4",
        board_to_fen(make_move("b4c3", EnPassantPosition))),

    TestBoard = fen_to_board("rnbqkb1r/pppppppp/5n2/3P4/8/8/PPP1PPPP/RNBQKBNR b KQkq - 0 2"),
    ?assertEqual("rnbqkb1r/pp1ppppp/5n2/2pP4/8/8/PPP1PPPP/RNBQKBNR w KQkq c6 0 3",
                 board_to_fen(make_move("c7c5", TestBoard))),

    ok.


-endif.
