type t;

type successOrFail = [ | `success | `fail];

module Fen: {
  type t;
  type error = {
    error_number: int,
    error: string,
  };
  let ofString: string => Js.Result.t(t, error);
  let ofStringExn: string => t;
};

type pgn = string;

let create: (~fen: Fen.t=?, unit) => t;

let loadFen: (t, Fen.t) => unit;

let fen: t => Fen.t;

let loadPgn: (~sloppy: bool=?, t, pgn) => successOrFail;

let pgn: t => pgn;

let pgnHeader: t => Js.Dict.t(string);

type kv = {
  key: string,
  value: string,
};

let addToPgnHeader: (t, kv) => unit;

let ascii: t => string;

module Color: {
  type t =
    | Black
    | White;
  let toString: t => string;
  let toJson: t => Js.Json.t;
};

module File: {
  type t = [ | `a | `b | `c | `d | `e | `f | `g | `h];
  let toChar: t => char;
  let ofChar: char => t;
  let ofString: string => t;
  let toString: t => string;
  let toJson: t => Js.Json.t;
  let all: list(t);
};

module Rank: {type t = int; let all: list(t); let toJson: t => Js.Json.t;};

module Square: {
  type t = {
    file: File.t,
    rank: Rank.t,
  };
  let toString: t => string;
  let ofString: string => t;
  let toJson: t => Js.Json.t;
};

module Piece: {
  module Type: {
    type t = [ | `king | `queen | `bishop | `knight | `rook | `pawn];
    let toString: t => string;
    let toJson: t => Js.Json.t;
  };
  type t = {
    type_: Type.t,
    color: Color.t,
  };
  let toString: t => string;
  let toJson: t => Js.Json.t;
};

module EndState: {
  type t =
    | Checkmate
    | Stalemate
    | ThreefoldRepetition
    | InsufficientMaterial
    | FiftyMoveRule;
  include Belt_Id.Hashable with type t := t;
  let toString: t => string;
};

let endState: t => option(EndState.t);

let inCheck: t => bool;

let gameOver: t => bool;

let inDraw: t => bool;

let get: (t, Square.t) => option(Piece.t);

module Move: {
  type san = string;
  module From_to: {
    type t = {
      from: string,
      to_: string,
    };
  };
  module Full: {
    type t = {
      color: Color.t,
      from: Square.t,
      to_: Square.t,
      flags: string,
      piece: Piece.Type.t,
      san,
    };
    let toJson: t => Js.Json.t;
  };
  [@bs.deriving accessors]
  type t =
    | SAN(string)
    | From_to(From_to.t)
    | Full(Full.t);
  module Options: {type t = {square: option(Square.t)};};
};

let legalMoves: (~move_options: Move.Options.t=?, t) => array(Move.Full.t);

let move: (t, Move.t) => option(Move.Full.t);

let undo: t => option(Move.Full.t);

let remove: (t, Square.t) => option(Piece.t);

let put: (t, Piece.t, Square.t) => successOrFail;

let turn: t => Color.t;

let historySan: t => array(Move.san);

let historyFull: t => array(Move.Full.t);

let reset: t => unit;
