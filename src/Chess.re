type chess;

type fen = string;

type piece;

type square = string;

/* module Move = { */
/*   type san = string; */
/*   type from_to = { */
/*     from: string, */
/*     to_: string, */
/*   }; */
/*   type t = */
/*     | SAN(san) */
/*     | FromTo(from_to); */
/* }; */
[@bs.val] [@bs.module "chess.js"]
external create : (~fen: fen=?, unit) => chess = "Chess";

[@bs.send] external ascii : chess => string = "";

[@bs.send] external fen : chess => fen = "";

let chess = create();

Js.log(ascii(chess));

Js.log(fen(chess));

let chess = {
  let fen = "r1k4r/p2nb1p1/2b4p/1p1n1p2/2PP4/3Q1NB1/1P3PPP/R5K1 b - c3 0 19";
  create(~fen, ());
};

Js.log(ascii(chess));

[@bs.send] external clear : chess => unit = "";

[@bs.send] external game_over : chess => Js.boolean = "";

let gameOver = chess => Js.to_bool(game_over(chess));

[@bs.send] external get : (chess, square) => piece = "";

type san = string; /* SAN is case sensitive */

type move;

[@bs.send] external move : (chess, move) => Js.nullable(move) = "";

[@bs.send] external moves : chess => array(move) = "";

Js.log("hey");

Js.log(ascii(chess));

Js.log(game_over(chess));

Js.log(get(chess, "e8"));

Js.log(get(chess, "e1"));

let play_random_game = (chess: chess) => {
  Js.log("about to play random game");
  Random.self_init();
  /* random game */
  let rec loop = chess =>
    if (gameOver(chess)) {
      Js.log("Game over");
    } else {
      let moves = moves(chess);
      let selected_move = moves[Random.int(Array.length(moves))];
      switch (Js.Nullable.toOption(move(chess, selected_move))) {
      | Some(_) =>
        Js.log(ascii(chess));
        loop(chess);
      | None => Js.log("error")
      };
    };
  loop(chess);
};

play_random_game(create());
/* %bs.raw */
/* {| */

   /* var Chess = require('chess.js').Chess; */
   /* var chess = new Chess(); */

   /* while (!chess.game_over()) { */
   /*   var moves = chess.moves(); */
   /*   var move = moves[Math.floor(Math.random() * moves.length)]; */
   /*   chess.move(move); */
   /* } */
   /* console.log(chess.pgn()); */

   /* |}; */
