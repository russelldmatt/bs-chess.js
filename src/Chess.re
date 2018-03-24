let unimplemented = x => {
  Js.log(x);
  Js.Exn.raiseError("Unimplemented");
};

module Raw = {
  /* Raw is a literal translation of the chess.js API */
  type chess;
  type fen = string;
  type pgn = string;
  type color = string;
  type piece = Js.Dict.t(string);
  type square = string;
  type san = string;
  type from_to = {
    .
    "from": string,
    "to": string,
  };
  [@bs.get_index]
  external getToFromFromTo : (from_to, [@bs.as "to"] _) => string = "";
  type full_move = {
    .
    "color": color,
    "from": square,
    "to": square,
    "flags": string,
    "piece": string, /* Not the same as piece type */
    "san": san,
  };
  [@bs.get_index]
  external getToFromFullMove : (full_move, [@bs.as "to"] _) => string = "";
  type header;
  type turn = string;
  type validation;
  [@bs.new] [@bs.module]
  external createForBrowser : (~fen: fen=?, unit) => chess = "chess.js";
  [@bs.new] [@bs.module "chess.js"]
  external createForNode : (~fen: fen=?, unit) => chess = "Chess";
  [@bs.send] external ascii : chess => string = "";
  [@bs.send] external clear : chess => unit = "";
  [@bs.send] external fen : chess => fen = "";
  [@bs.send] external game_over : chess => bool = "";
  [@bs.send] external get : (chess, square) => Js.nullable(piece) = "";
  type verbose = {. "verbose": bool};
  [@bs.send] external history : chess => array(san) = "";
  [@bs.send]
  external history_verbose : (chess, verbose) => array(full_move) = "history";
  [@bs.send] external in_check : chess => bool = "";
  [@bs.send] external in_checkmate : chess => bool = "";
  [@bs.send] external in_draw : chess => bool = "";
  [@bs.send] external in_stalemate : chess => bool = "";
  [@bs.send] external in_threefold_repetition : chess => bool = "";
  [@bs.send] external addToHeader : (chess, string, string) => unit = "header";
  [@bs.send] external header : chess => header = "";
  [@bs.send] external insufficient_material : chess => bool = "";
  [@bs.send] external load : (chess, fen) => unit = "";
  type sloppy = {. "sloppy": bool};
  [@bs.send]
  external load_pgn : (chess, pgn, Js.nullable(sloppy), unit) => bool = "";
  [@bs.send]
  external move_san : (chess, san) => Js.nullable(full_move) = "move";
  [@bs.send]
  external move_from_to : (chess, from_to) => Js.nullable(full_move) = "move";
  /* The options you can pass to "moves" are too flexible to represent
     nicely so I'm just going to represent it as a few different
     functions. */
  [@bs.send] external moves : chess => array(san) = "moves";
  [@bs.send]
  external moves_verbose : (chess, verbose) => array(full_move) = "moves";
  type for_square = {. "square": square};
  type for_square_verbose = {
    .
    "square": square,
    "verbose": bool,
  };
  [@bs.send]
  external moves_for_square : (chess, for_square) => array(san) = "moves";
  [@bs.send]
  external moves_for_square_verbose :
    (chess, for_square_verbose) => array(full_move) =
    "moves";
  [@bs.send] external pgn : chess => pgn = "";
  [@bs.send] external put : (chess, piece, square) => bool = "";
  [@bs.send] external remove : (chess, square) => Js.nullable(piece) = "";
  [@bs.send] external reset : chess => unit = "";
  [@bs.send] external square_color : (chess, square) => color = "";
  [@bs.send] external turn : chess => turn = "";
  [@bs.send] external undo : chess => Js.nullable(full_move) = "";
  [@bs.send] external validate_fen : (chess, fen) => validation = "";
};

module List = {
  include List;
  let init = (n, f) => Array.init(n, f) |> Array.to_list;
  let bind = (f, l) => l |> List.map(f) |> List.concat;
  let filter_map = Belt.List.keepMap;
};

module String = {
  include String;
  let ofChar = c => String.make(1, c);
};

module Api = {
  /* Api is the api that I'd like to expose.  chess.js, ocamlified. */
  type fen = Raw.fen;
  type pgn = Raw.pgn;
  module Color = {
    type t =
      | Black
      | White;
    let ofRaw: Raw.color => t =
      raw =>
        switch (raw) {
        | "b" => Black
        | "w" => White
        | raw =>
          Js.Exn.raiseError(
            "Cannot convert color from raw (raw: " ++ raw ++ ")",
          )
        };
    let toRaw = t =>
      switch (t) {
      | White => "w"
      | Black => "b"
      };
    let toString = t =>
      switch (t) {
      | Black => "black"
      | White => "white"
      };
  };
  module File = {
    type t = [ | `a | `b | `c | `d | `e | `f | `g | `h];
    let toChar = t =>
      switch (t) {
      | `a => 'a'
      | `b => 'b'
      | `c => 'c'
      | `d => 'd'
      | `e => 'e'
      | `f => 'f'
      | `g => 'g'
      | `h => 'h'
      };
    let ofChar = c =>
      switch (c) {
      | 'a' => `a
      | 'b' => `b
      | 'c' => `c
      | 'd' => `d
      | 'e' => `e
      | 'f' => `f
      | 'g' => `g
      | 'h' => `h
      | c =>
        Js.Exn.raiseError(
          "Cannot convert File.t from char (char: " ++ String.ofChar(c) ++ ")",
        )
      };
    let ofString = s => ofChar(s.[0]);
    let toString = t => t |> toChar |> String.ofChar;
    let all: list(t) = [`a, `b, `c, `d, `e, `f, `g, `h];
  };
  module Square = {
    type t = {
      file: File.t,
      rank: int,
    };
    let toString = t => {
      let {file, rank} = t;
      File.toString(file) ++ string_of_int(rank);
    };
    let ofString = s => {
      file: s.[0] |> File.ofChar,
      rank: s.[1] |> String.ofChar |> int_of_string,
    };
  };
  module Piece = {
    module Type = {
      type t = [ | `king | `queen | `bishop | `knight | `rook | `pawn];
      let ofRaw = raw =>
        switch (String.lowercase(raw)) {
        | "k" => `king
        | "q" => `queen
        | "b" => `bishop
        | "n" => `knight
        | "r" => `rook
        | "p" => `pawn
        | raw =>
          Js.Exn.raiseError(
            "Cannot convert piece from raw (raw: " ++ raw ++ ")",
          )
        };
      let toRaw = t =>
        switch (t) {
        | `king => "k"
        | `queen => "q"
        | `bishop => "b"
        | `knight => "n"
        | `rook => "r"
        | `pawn => "p"
        };
      let toString = t =>
        switch (t) {
        | `king => "king"
        | `queen => "queen"
        | `bishop => "bishop"
        | `knight => "knight"
        | `rook => "rook"
        | `pawn => "pawn"
        };
    };
    type t = {
      type_: Type.t,
      color: Color.t,
    };
    let ofRaw: Raw.piece => t =
      raw => {
        type_: Js.Dict.get(raw, "type") |> Js.Option.getExn |> Type.ofRaw,
        color: Color.ofRaw(Js.Dict.get(raw, "color") |> Js.Option.getExn),
      };
    let toString = t =>
      Color.toString(t.color) ++ " " ++ Type.toString(t.type_);
  };
  type t = Raw.chess;
  let create = (~fen=?, ()) =>
    switch ([%external window]) {
    | None => Raw.createForNode(~fen?, ())
    | Some(_) => Raw.createForBrowser(~fen?, ())
    };
  let ascii = Raw.ascii;
  let fen = Raw.fen;
  let gameOver = Raw.game_over;
  let get: (t, Square.t) => option(Piece.t) =
    (t, square) => {
      let rawSquare = square |> Square.toString;
      Js.log2("raw square: ", rawSquare);
      let raw = Raw.get(t, rawSquare) |> Js.Nullable.toOption;
      Belt.Option.map(raw, Piece.ofRaw);
    };
  module Move = {
    type san = string;
    module From_to = {
      type t = {
        from: string,
        to_: string,
      };
      let toRaw: t => Raw.from_to = t => {"from": t.from, "to": t.to_};
    };
    module Full = {
      type t = {
        color: Color.t,
        from: Square.t,
        to_: Square.t,
        flags: string,
        piece: Piece.Type.t,
        san,
      };
      let ofRaw: Raw.full_move => t =
        raw => {
          color: Color.ofRaw(raw##color),
          from: Square.ofString(raw##from),
          to_: Square.ofString(Raw.getToFromFullMove(raw)),
          flags: raw##flags,
          piece: Piece.Type.ofRaw(raw##piece),
          san: raw##san,
        };
      let toRaw: t => Raw.full_move =
        t => {
          "color": Color.toRaw(t.color),
          "from": Square.toString(t.from),
          "to": Square.toString(t.to_),
          "flags": t.flags,
          "piece": Piece.Type.toRaw(t.piece),
          "san": t.san,
        };
    };
    type t =
      | SAN(string)
      | From_to(From_to.t)
      | Full(Full.t);
    module Options = {
      type t = {square: option(Square.t)};
    };
  };
  let legalMoves = (~move_options=?, t) => {
    let square = Belt.Option.flatMap(move_options, x => x.Move.Options.square);
    let raw_full_moves =
      switch (square) {
      | None => Raw.moves_verbose(t, {"verbose": true})
      | Some(square) =>
        Raw.moves_for_square_verbose(
          t,
          {"square": Square.toString(square), "verbose": true},
        )
      };
    Js.log(raw_full_moves);
    Array.map(Move.Full.ofRaw, raw_full_moves);
  };
  let move: (t, Move.t) => option(Move.Full.t) =
    (t, req) => {
      let parseResponse = r =>
        r |> Js.Nullable.toOption |. Belt.Option.map(Move.Full.ofRaw);
      switch (req) {
      | Move.SAN(san) => Raw.move_san(t, san) |> parseResponse
      | From_to(from_to) =>
        Move.From_to.toRaw(from_to) |> Raw.move_from_to(t) |> parseResponse
      | Full(full) => Raw.move_san(t, full.san) |> parseResponse
      };
    };
  let loadPgn = (~sloppy=?, t, pgn) => {
    let sloppy =
      Belt.Option.map(sloppy, s => {"sloppy": s}) |> Js.Nullable.fromOption;
    Raw.load_pgn(t, pgn, sloppy, ());
  };
};

include Api;

module Tests = {
  let chess = create();
  Js.log(ascii(chess));
  Js.log(fen(chess));
  let chess = {
    let fen = "6nr/3p2pp/5Q2/3b2B1/1nk1PPBP/p4Np1/p5R1/1R1K1N2 b - - 2 40";
    create(~fen, ());
  };
  let full_move = {
    Move.Full.color: Color.Black,
    from: {
      Square.file: `a,
      rank: 2,
    },
    to_: {
      Square.file: `a,
      rank: 1,
    },
    flags: "np",
    piece: `pawn,
    san: "a1=B",
  };
  /* Move.Full.ofRaw( */
  /*   [%raw {|{ color: 'b', from: 'f2', to: 'f1', flags: 'np', piece: 'p' }|}], */
  /* ); */
  Js.log(ascii(chess));
  Js.log(Move.Full.toRaw(full_move));
  Js.log(
    move(chess, Move.Full(full_move)) |. Belt.Option.map(Move.Full.toRaw),
  );
  Js.log(ascii(chess));
  Js.log(ascii(chess));
  Js.log("hey");
  Js.log(ascii(chess));
  Js.log(gameOver(chess));
  Js.log(get(chess, {Square.file: `e, rank: 8}));
  Js.log(get(chess, {Square.file: `e, rank: 1}));
  Js.log(
    get(chess, {Square.file: `g, rank: 2})
    |. Belt.Option.map(Piece.toString),
  );
  let allSquares: list(Square.t) = {
    let allRanks = List.init(8, x => x + 1);
    File.all
    |> List.bind(file => allRanks |> List.map(rank => {Square.file, rank}));
  };
  Js.log("gettin all squares");
  let allPieces: list((Square.t, Piece.t)) =
    allSquares
    |. List.filter_map(square =>
         Belt.Option.map(get(chess, square), piece => (square, piece))
       );
  Js.log(ascii(chess));
  allPieces
  |> List.iter(((square, piece)) =>
       Js.log3(Square.toString(square), ":", Piece.toString(piece))
     );
  let play_random_game = (t: t) => {
    Js.log("about to play random game");
    Random.self_init();
    /* random game */
    let rec loop = t => {
      Js.log(fen(t));
      Js.log(ascii(t));
      if (gameOver(t)) {
        Js.log("Game over");
      } else {
        let moves = legalMoves(t);
        /* Js.log("legal moves:"); */
        /* Array.iter(Js.log, Array.map(Move.Full.toRaw, moves)); */
        let selected_move = moves[Random.int(Array.length(moves))];
        Js.log("selecting move:");
        Js.log(Move.Full.toRaw(selected_move));
        switch (move(t, Move.Full(selected_move))) {
        | Some(_) => loop(t)
        | None => Js.log("error")
        };
      };
    };
    loop(t);
  };
  play_random_game(create());
};
