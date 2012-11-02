% move these records to an hrl file later

-type chesspiece() :: pawn | bishop | knight | rook | queen | king.
-type chesscolor() :: white | black.
-type chessmove_special() :: normal | castle | promotion | enpassant.
-type chesssquare() :: 1..64.

-define(STARTPOS_FEN, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").

-record(castling_rights, {
        white_kingside = true :: boolean(),
        white_queenside = true :: boolean(),
        black_kingside = true :: boolean(),
        black_queenside = true :: boolean()
        }).

-record(chessboard, {
        placements :: tuple(),
        active = white :: chesscolor(),
        castling = #castling_rights{} :: tuple(),
        enpassant = none :: chesssquare() | none,
        halfmove_clock = 0 :: pos_integer(),
        fullmove_number = 1 :: pos_integer()
        }).
-record(chessmove, {
        side :: chesscolor(),
        chesspiece :: chesspiece(),
        from :: chesssquare(),
        to :: chesssquare(),
        special :: chessmove_special(),
        capture = false :: boolean(),
        promotion = undefined :: chesspiece() | undefined
        }).
