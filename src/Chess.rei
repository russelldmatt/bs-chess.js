/* Api is the api that I'd like to expose.  chess.js, ocamlified. */
type t;

type fen = string;

type pgn = string;

module Color: {
  type t =
    | Black
    | White;
  let toString: t => string;
};

module File: {
  type t = [ | `a | `b | `c | `d | `e | `f | `g | `h];
  let toChar: t => char;
  let ofChar: char => t;
  let ofString: string => t;
  let toString: t => string;
  let all: list(t);
};

module Square: {
  type t = {
    file: File.t,
    rank: int,
  };
  let toString: t => string;
  let ofString: string => t;
};

module Piece: {
  module Type: {
    type t = [ | `king | `queen | `bishop | `knight | `rook | `pawn];
    let toString: t => string;
  };
  type t = {
    type_: Type.t,
    color: Color.t,
  };
  let toString: t => string;
};

let create: (~fen: fen=?, unit) => t;

let ascii: t => string;

let fen: t => fen;

let gameOver: t => bool;

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
  };
  type t =
    | SAN(string)
    | From_to(From_to.t)
    | Full(Full.t);
  module Options: {type t = {square: option(Square.t)};};
};

let legalMoves: (~move_options: Move.Options.t=?, t) => array(Move.Full.t);

let move: (t, Move.t) => option(Move.Full.t);

let loadPgn: (~sloppy: bool=?, t, pgn) => bool;
