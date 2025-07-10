open! Core

type t =
  | Illegal_move
  | Game_continues
  | Game_over of { winner : Piece.t option }
[@@deriving sexp_of]
(* 
let equal eval1 eval2 =
  match (eval1, eval2) with
  | Illegal_move, Illegal_move | Game_continues, Game_continues -> true
  | Game_over { winner = winner1 }, Game_over { winner = winner2 } -> (
      match (winner1, winner2) with
      | Some Piece.X, Some Piece.X | Some Piece.O, Some Piece.O -> true
      | _, _ -> false)
  | _ -> false *)

let terminating eval = 
  match eval with
  | Game_continues -> false
  | _ -> true