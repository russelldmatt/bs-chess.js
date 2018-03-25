/* let unimplemented = x => { */
/*   Js.log(x); */
/*   Js.Exn.raiseError("Unimplemented"); */
/* }; */
module Raw = {
  /* Raw is a literal translation of the chess.js API */
  type chess;
  type fen = string;
  type pgn = string;
  type color = string;
  type piece = {
    .
    "_type": string,
    "color": string,
  };
  type square = string;
  type san = string;
  type from_to = {
    .
    "from": string,
    "to": string,
  };
  type full_move = {
    .
    "color": color,
    "from": square,
    "_to": square,
    "flags": string,
    "piece": string, /* Not the same as piece type */
    "san": san,
  };
  type header = Js.Dict.t(string);
  type validation = Js.Dict.t(string);
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
  [@bs.send] external load : (chess, fen) => bool = "";
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
  [@bs.send] external turn : chess => color = "";
  [@bs.send] external undo : chess => Js.nullable(full_move) = "";
  [@bs.send] external validate_fen : (chess, fen) => validation = "";
};

module List = {
  include List;
  let init = (n, f) => Array.init(n, f) |> Array.to_list;
};

module String = {
  include String;
  let ofChar = c => String.make(1, c);
};

module Api = {
  module SuccessOrFail = {
    type t = [ | `success | `fail];
    let ofBool = b : t => b ? `success : `fail;
  };
  type successOrFail = SuccessOrFail.t;
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
  module Rank = {
    type t = int;
    let all = List.init(8, x => x + 1);
  };
  module Square = {
    type t = {
      file: File.t,
      rank: Rank.t,
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
        type_: raw##_type |> Type.ofRaw,
        color: Color.ofRaw(raw##color),
      };
    let toRaw = t : Raw.piece => {
      "_type": t.type_ |> Type.toRaw,
      "color": t.color |> Color.toRaw,
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
  let get: (t, Square.t) => option(Piece.t) =
    (t, square) => {
      let rawSquare = square |> Square.toString;
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
      /* CR mrussell: Want to expose the tToJs function without exposing another full type. */
      [@bs.deriving jsConverter]
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
          to_: Square.ofString(raw##_to),
          flags: raw##flags,
          piece: Piece.Type.ofRaw(raw##piece),
          san: raw##san,
        };
      let toRaw: t => Raw.full_move =
        t => {
          "color": Color.toRaw(t.color),
          "from": Square.toString(t.from),
          "_to": Square.toString(t.to_),
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
    Raw.load_pgn(t, pgn, sloppy, ()) |> SuccessOrFail.ofBool;
  };
  module EndState = {
    module T = {
      type t =
        | Checkmate
        | Stalemate
        | ThreefoldRepetition
        | InsufficientMaterial
        | FiftyMoveRule;
      /* CR mrussell: easier way? */
      let hash = t : int =>
        switch (t) {
        | Checkmate => 0
        | Stalemate => 1
        | ThreefoldRepetition => 2
        | InsufficientMaterial => 3
        | FiftyMoveRule => 4
        };
      let eq = (t1, t2) => t1 == t2;
    };
    include T;
    include (Belt_Id.MakeHashable(T): Belt_Id.Hashable with type t := t);
    let toString = t : string =>
      switch (t) {
      | Checkmate => "Checkmate"
      | Stalemate => "Stalemate"
      | ThreefoldRepetition => "ThreefoldRepetition"
      | InsufficientMaterial => "InsufficientMaterial"
      | FiftyMoveRule => "FiftyMoveRule"
      };
  };
  let inCheck = Raw.in_check;
  let gameOver = Raw.game_over;
  let inDraw = Raw.in_draw;
  let endState: t => option(EndState.t) =
    t =>
      if (Raw.game_over(t)) {
        Some(
          Raw.in_checkmate(t) ?
            Checkmate :
            Raw.in_stalemate(t) ?
              Stalemate :
              Raw.in_threefold_repetition(t) ?
                ThreefoldRepetition :
                Raw.insufficient_material(t) ?
                  InsufficientMaterial :
                  inDraw(t) ?
                    FiftyMoveRule :
                    Js.Exn.raiseError(
                      "LIBRARY BUG: game is over but can't detect the end state",
                    ),
        );
      } else {
        None;
      };
  let undo = t =>
    t |> Raw.undo |> Js.Nullable.toOption |. Belt.Option.map(Move.Full.ofRaw);
  type kv = {
    key: string,
    value: string,
  };
  let addToPgnHeader = (t, key_value) =>
    Raw.addToHeader(t, key_value.key, key_value.value);
  let pgnHeader = Raw.header;
  let pgn = Raw.pgn;
  let turn = t => t |> Raw.turn |> Color.ofRaw;
  let remove = (t, square) =>
    Raw.remove(t, Square.toString(square))
    |> Js.Nullable.toOption
    |. Belt.Option.map(Piece.ofRaw);
  let put = (t, piece, square) =>
    Raw.put(t, Piece.toRaw(piece), Square.toString(square))
    |> SuccessOrFail.ofBool;
  let validateFen = Raw.validate_fen;
  let loadFen = (t, fen) : Js.Result.t(unit, Js.Dict.t(string)) =>
    Raw.load(t, fen) ? Ok() : Error(validateFen(t, fen));
  let historySan = t => Raw.history(t);
  let historyFull = t =>
    Raw.history_verbose(t, {"verbose": true})
    |> Array.map(raw_full_move => Move.Full.ofRaw(raw_full_move));
  let reset = Raw.reset;
  /* Explicitly ignore a few raw things that don't seem that useful  */
  let _ = Raw.square_color; /* When would you need that? */
  let _ = Raw.moves_for_square; /* Just always use verbose */
  let _ = Raw.moves; /* Just always use verbose */
  let _ = Raw.clear; /* Just create a new one? */
};

include Api;
