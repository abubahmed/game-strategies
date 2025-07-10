open! Core
open! Async
open! Game_strategies_common_lib

(* This is a helper function for constructing games from a list of positions *)
let init_game (board : (Position.t * Piece.t) list) : Game.t =
  { (Game.empty Tic_tac_toe) with board = Position.Map.of_alist_exn board }

let win_for_x =
  init_game
    [
      ({ row = 0; column = 0 }, X);
      ({ row = 1; column = 0 }, O);
      ({ row = 2; column = 2 }, X);
      ({ row = 2; column = 0 }, O);
      ({ row = 2; column = 1 }, X);
      ({ row = 1; column = 1 }, O);
      ({ row = 0; column = 2 }, X);
      ({ row = 0; column = 1 }, O);
      ({ row = 1; column = 2 }, X);
    ]

let non_win =
  init_game [ ({ row = 0; column = 0 }, O); ({ row = 2; column = 2 }, O) ]

let print_game (game : Game.t) =
  let board_dim = Game_kind.board_length game.game_kind in
  let game_board = game.board in

  let _ =
    List.init board_dim ~f:(fun row ->
        let _ =
          List.init board_dim ~f:(fun col ->
              let row = board_dim - 1 - row in
              let col = board_dim - 1 - col in
              (* print_endline (Int.to_string row ^ Int.to_string col); *)
              let this_pos = Map.find game_board { row; column = col } in
              let _ =
                match this_pos with
                | Some value -> (
                    match value with
                    | Piece.O -> printf "O"
                    | Piece.X -> printf "X")
                | None -> printf " "
              in
              if col = board_dim - 1 then () else printf " | ")
        in
        if row = 0 || row = board_dim then ()
        else (
          print_newline ();
          print_endline "---------"))
  in
  ()

let%expect_test "print_win_for_x" =
  print_game win_for_x;
  [%expect
    {|
      X | O | X
      ---------
      O | O | X
      ---------
      O | X | X
      |}];
  return ()

let%expect_test "print_non_win" =
  print_game non_win;
  [%expect
    {|
      X |   |
      ---------
      O |   |
      ---------
      O |   | X
      |}];
  return ()

(* Exercise 1 *)
let available_moves (game : Game.t) : Position.t list =
  let board_dim = Game_kind.board_length game.game_kind in
  let game_board = game.board in

  let all_positions =
    List.init board_dim ~f:(fun row ->
        List.init board_dim ~f:(fun col ->
            let row = board_dim - 1 - row in
            let col = board_dim - 1 - col in
            let this_pos = Map.find game_board { row; column = col } in
            match this_pos with
            | Some value -> (
                match value with
                | Piece.O -> { Position.row = -1; Position.column = -1 }
                | Piece.X -> { Position.row = -1; Position.column = -1 })
            | None -> { row; column = col }))
  in
  List.filter (List.concat all_positions) ~f:(fun { row; column } ->
      (not (row = -1)) && not (column = -1))

let%expect_test "available_moves" =
  let availables = available_moves non_win in
  List.iter availables ~f:(fun pos ->
      print_endline (Position.to_string pos);
      ());
  [%expect {||}];
  return ()

let check_list_for_win list =
  let all_x =
    List.for_all list ~f:(fun piece ->
        match piece with Some Piece.X -> true | _ -> false)
  in
  let all_o =
    List.for_all list ~f:(fun piece ->
        match piece with Some Piece.O -> true | _ -> false)
  in
  if all_o then Some Piece.O else if all_x then Some Piece.X else None

let get_pieces_direction game_board row col win_length (down : int)
    (right : int) =
  List.init win_length ~f:(fun pos ->
      let pos = win_length - 1 - pos in
      let increment_row =
        if down = 1 then row + pos else if down = -1 then row - pos else row
      in
      let increment_col =
        if right = 1 then col + pos else if right = -1 then col - pos else col
      in
      Map.find game_board
        { Position.row = increment_row; Position.column = increment_col })

let check_for_win (game : Game.t) =
  let board_dim = Game_kind.board_length game.game_kind in
  let game_board = game.board in
  let win_length = Game_kind.win_length game.game_kind in

  let results =
    List.init board_dim ~f:(fun row ->
        List.init board_dim ~f:(fun col ->
            let row = board_dim - 1 - row in
            let col = board_dim - 1 - col in

            let down_pieces =
              get_pieces_direction game_board row col win_length 1 0
            in
            let winner_down = check_list_for_win down_pieces in

            let right_pieces =
              get_pieces_direction game_board row col win_length 0 1
            in
            let winner_right = check_list_for_win right_pieces in

            let right_diag_pieces =
              get_pieces_direction game_board row col win_length 1 1
            in
            let winner_right_diagonal = check_list_for_win right_diag_pieces in

            let left_diag_pieces =
              get_pieces_direction game_board row col win_length 1 (-1)
            in
            let winner_left_diagonal = check_list_for_win left_diag_pieces in

            match winner_right_diagonal with
            | Some piece -> Some piece
            | None -> (
                match winner_left_diagonal with
                | Some piece -> Some piece
                | None -> (
                    match winner_down with
                    | Some piece -> Some piece
                    | None -> (
                        match winner_right with
                        | Some piece -> Some piece
                        | None -> None)))))
  in
  List.fold (List.concat results) ~init:None ~f:(fun acc result ->
      match result with
      | None -> acc
      | Some piece -> Some (Evaluation.Game_over { winner = Some piece }))

(* Exercise 2 *)
let evaluate (game : Game.t) : Evaluation.t =
  let board_dim = Game_kind.board_length game.game_kind in
  let game_board = game.board in

  let is_illegal =
    Map.fold game_board ~init:false ~f:(fun ~key:{ row; column } ~data:_ acc ->
        if
          row > board_dim - 1 || row < 0 || column > board_dim - 1 || column < 0
        then true
        else acc)
  in

  if is_illegal then Evaluation.Illegal_move
  else
    match check_for_win game with
    | Some winner -> winner
    | None ->
        let remaining_moves = List.length (available_moves game) > 0 in
        if remaining_moves then Evaluation.Game_continues
        else Evaluation.Game_over { winner = None }

(* Exercise 3 *)
let winning_moves ~(me : Piece.t) (game : Game.t) : Position.t list =
  let available_moves = available_moves game in
  List.fold available_moves ~init:[] ~f:(fun acc position ->
      let new_board_game = Game.set_piece game position me in
      match evaluate new_board_game with
      | Evaluation.Game_over { winner = Some this_me } ->
          if Piece.equal this_me me then acc @ [ position ] else acc
      | _ -> acc)

(* Exercise 4 *)
let losing_moves ~(me : Piece.t) (game : Game.t) : Position.t list =
  match me with
  | Piece.X -> winning_moves ~me:Piece.O game
  | Piece.O -> winning_moves ~me:Piece.X game

let exercise_one =
  Command.async ~summary:"Exercise 1: Where can I move?"
    (let%map_open.Command () = return () in
     fun () ->
       let moves = available_moves win_for_x in
       print_s [%sexp (moves : Position.t list)];
       let moves = available_moves non_win in
       print_s [%sexp (moves : Position.t list)];
       return ())

let exercise_two =
  Command.async ~summary:"Exercise 2: Is the game over?"
    (let%map_open.Command () = return () in
     fun () ->
       let evaluation = evaluate win_for_x in
       print_s [%sexp (evaluation : Evaluation.t)];
       let evaluation = evaluate win_for_x in
       print_s [%sexp (evaluation : Evaluation.t)];
       return ())

let piece_flag =
  let open Command.Param in
  flag "piece"
    (required (Arg_type.create Piece.of_string))
    ~doc:
      ("PIECE "
      ^ (Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "))

let exercise_three =
  Command.async ~summary:"Exercise 3: Is there a winning move?"
    (let%map_open.Command () = return () and piece = piece_flag in
     fun () ->
       let winning_moves = winning_moves ~me:piece non_win in
       print_s [%sexp (winning_moves : Position.t list)];
       return ())

let exercise_four =
  Command.async ~summary:"Exercise 4: Is there a losing move?"
    (let%map_open.Command () = return () and piece = piece_flag in
     fun () ->
       let losing_moves = losing_moves ~me:piece non_win in
       print_s [%sexp (losing_moves : Position.t list)];
       return ())

let available_moves_that_do_not_immediately_lose ~(me : Piece.t) (game : Game.t)
    : Position.t list =
  let available_moves = available_moves game in
  let losing_moves = losing_moves ~me game in
  let winning_moves = winning_moves ~me game in

  match losing_moves with
  | [] -> available_moves
  | _ ->
      List.dedup_and_sort ~compare:Position.compare
        (losing_moves @ winning_moves)

let exercise_six =
  Command.async ~summary:"Exercise 6: all non-losing moves"
    (let%map_open.Command () = return () and piece = piece_flag in
     fun () ->
       let nonlosing_moves =
         available_moves_that_do_not_immediately_lose ~me:piece non_win
       in
       print_s [%sexp (nonlosing_moves : Position.t list)];
       return ())

let command =
  Command.group ~summary:"Exercises"
    [
      ("one", exercise_one);
      ("two", exercise_two);
      ("three", exercise_three);
      ("four", exercise_four);
      ("six", exercise_six);
    ]

let update_availables (availables : Position.t list)
    ({ row; column } : Position.t) =
  List.filter availables
    ~f:(fun { Position.row = avail_row; Position.column = avail_column } ->
      not (row = avail_row && column = avail_column))

let score ~(me : Piece.t) (game : Game.t) : Float.t =
  match evaluate game with
  | Evaluation.Game_over { winner = Some winner } ->
      if Piece.equal me winner then Float.infinity else Float.neg_infinity
  | _ -> 0.0

let rec minimax ~(player : Piece.t) ~(game : Game.t) ~(depth : int)
    ~(max_player : bool) ~(available_moves : Position.t list) =
  let other_player = Piece.flip player in

  if List.length available_moves = 0 || depth = 0 then score ~me:player game
  else if max_player then
    List.fold available_moves ~init:Float.neg_infinity ~f:(fun acc position ->
        let moved_game = Game.set_piece game position player in
        let new_available_moves = update_availables available_moves position in
        let lower_minimax_value =
          minimax ~player ~game:moved_game ~depth:(depth - 1) ~max_player:false
            ~available_moves:new_available_moves
        in
        if Float.(lower_minimax_value > acc) then lower_minimax_value else acc)
  else
    List.fold available_moves ~init:Float.infinity ~f:(fun acc position ->
        let moved_game = Game.set_piece game position other_player in
        let new_available_moves = update_availables available_moves position in
        let lower_minimax_value =
          minimax ~player ~game:moved_game ~depth:(depth - 1) ~max_player:true
            ~available_moves:new_available_moves
        in
        if Float.(lower_minimax_value < acc) then lower_minimax_value else acc)

let minimax_algo ~(player : Piece.t) ~(game : Game.t) ~(depth : int) =
  let available_moves = available_moves game in
  let dummy_pos = { Position.row = -1; Position.column = -1 } in
  let _, winner =
    List.fold available_moves ~init:(Float.neg_infinity, dummy_pos)
      ~f:(fun (acc_val, acc_pos) position ->
        let moved_game = Game.set_piece game position player in
        let new_available_moves = update_availables available_moves position in
        let minimax_value =
          minimax ~player ~game:moved_game ~depth:(depth - 1) ~max_player:false
            ~available_moves:new_available_moves
        in
        if Float.(minimax_value > acc_val) then (minimax_value, position)
        else (acc_val, acc_pos))
  in
  winner

(* Exercise 5 *)
let make_move ~(game : Game.t) ~(you_play : Piece.t) : Position.t =
  let winning_moves = winning_moves ~me:you_play game in
  match List.random_element winning_moves with
  | Some winning_pos -> winning_pos
  | None -> (
      let losing_moves = losing_moves ~me:you_play game in
      match List.random_element losing_moves with
      | Some losing_pos -> losing_pos
      | None ->
          let call_minimax = minimax_algo ~player:you_play ~game ~depth:2 in
          call_minimax)
