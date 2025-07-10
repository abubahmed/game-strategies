open! Core

type t =
  | Illegal_move
  | Game_continues
  | Game_over of { winner : Piece.t option }
[@@deriving sexp_of]

val terminating : t -> bool
