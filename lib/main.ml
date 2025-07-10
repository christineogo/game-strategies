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
  init_game
    [
      ({ row = 0; column = 0 }, X);
      ({ row = 1; column = 0 }, O);
      ({ row = 2; column = 2 }, X);
      ({ row = 2; column = 0 }, O);
    ]

let piece_to_string = function Piece.X -> "X" | O -> "O"

let print_game (game : Game.t) =
  let game_size = Game_kind.board_length game.game_kind in
  (* let board_skeleton = List.init (Game_kind.board_length game.game_kind + (Game_kind.board_length game.game_kind*3)) ~f:(fun _ -> "_") in *)

  (* replace with List.init to create a list frmo 0 to 1- game_size and then and list.iter on that list and do the inner function*)
  for row = 0 to game_size - 1 do
    let row_pieces =
      List.init game_size ~f:(fun col ->
          let piece_pos = { Position.row; column = col } in
          match Map.find game.board piece_pos with
          | Some piece -> piece_to_string piece
          | None -> " ")
    in
    let row_str = String.concat ~sep:" | " row_pieces in
    print_endline row_str;
    (* to generalize, replace below with String.make with length determined by formula *)
    if row < game_size - 1 then print_endline "---------"
  done
(* print_endline "X |   |\n---------\nO |   |\n---------\nO |   | X" *)

let%expect_test "print_win_for_x" =
  print_game win_for_x;
  [%expect
    {|
      X | O | X
      ---------
       | O | 
      ---------
      O |  | 
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
  let game_size = Game_kind.board_length game.game_kind in
  let board_pieces =
    (* to get rid of concats I can use list.init to create all positions in a row. Call list.filter to filter out elements in the map *)
    List.init game_size ~f:(fun row ->
        let row_pieces =
          List.init game_size ~f:(fun col ->
              let piece_pos = { Position.row; column = col } in
              (* use map.mem to avoid matching *)
              match Map.find game.board piece_pos with
              | Some _ -> []
              | None -> [ piece_pos ])
        in
        List.concat row_pieces)
  in
  List.concat board_pieces

(* Map.filter game.board ~f:(fun )
  failwith "Implement me!" *)

(* Exercise 2 *)

let check_direction ~start ~change ~length ~(game : Game.t) ~(piece : Piece.t) :
    (* position.ml already has the direction functions -- use recursion with a max depth to check *)
    bool =
  let game_kind = game.game_kind in
  List.init length ~f:(fun step ->
      let pos =
        {
          Position.row = start.Position.row + (step * change.Position.row);
          column = start.Position.column + (step * change.Position.column);
        }
      in
      Position.in_bounds ~game_kind pos
      &&
      match Map.find game.board pos with
      | Some board_piece -> Piece.equal board_piece piece
      | None -> false)
  |> List.for_all ~f:Fun.id

let check_for_win (position : Position.t) (game : Game.t) (piece : Piece.t) :
    bool =
  let size = Game_kind.board_length game.game_kind in
  let directions =
    [
      { Position.row = 0; column = 1 };
      { Position.row = 1; column = 0 };
      { Position.row = 1; column = 1 };
      (* Diagonal right*)
      { Position.row = -1; column = 1 };
      (* Diagonal left*)
    ]
  in

  List.exists directions ~f:(fun change ->
      check_direction ~start:position ~change ~length:size ~game ~piece)

let evaluate (game : Game.t) : Evaluation.t =
  (* ignore game;
  failwith "Implement me!" *)
  if
    Map.existsi game.board ~f:(fun ~key:position ~data:_ ->
        not (Position.in_bounds ~game_kind:game.game_kind position))
  then Evaluation.Illegal_move
  else
    let potential_winner =
      (* use Map.fold_until to decide when to stop *)
      Map.fold game.board ~init:None ~f:(fun ~key:position ~data:piece acc ->
          match acc with
          | Some _ -> acc
          | None ->
              if check_for_win position game piece then Some piece else None)
    in
    match potential_winner with
    | Some piece -> Evaluation.Game_over { winner = Some piece }
    | None ->
        if List.is_empty (available_moves game) then
          Evaluation.Game_over { winner = None }
        else Evaluation.Game_continues

(* Exercise 3 *)
let winning_moves ~(me : Piece.t) (game : Game.t) : Position.t list =
  List.filter (available_moves game) ~f:(fun position ->
      match evaluate (Game.set_piece game position me) with
      | Evaluation.Game_over { winner = Some winning_piece } ->
          Piece.equal winning_piece me
      | _ -> false)

let%expect_test "winning move" =
  let moves = winning_moves non_win ~me:Piece.X in
  print_s [%sexp (moves : Position.t list)];
  [%expect {|(((row 1) (column 1)))|}];
  return ()

(* Exercise 4 *)
let losing_moves ~(me : Piece.t) (game : Game.t) : Position.t list =
  (* instead of converting sets, create set of winning moves and filter the available moves *)
  let non_winning_set =
    Set.diff
      (Position.Set.of_list (available_moves game))
      (Position.Set.of_list (winning_moves ~me game))
  in
  let non_winning_list = Set.to_list non_winning_set in
  List.filter non_winning_list ~f:(fun position ->
      not
        (List.is_empty
           (winning_moves ~me:(Piece.flip me) (Game.set_piece game position me))))

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
       let evaluation = evaluate non_win in
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

let command =
  Command.group ~summary:"Exercises"
    [
      ("one", exercise_one);
      ("two", exercise_two);
      ("three", exercise_three);
      ("four", exercise_four);
    ]

(* Exercise 5 *)
let rec minimax ~position ~player game =
  let new_game = Game.set_piece game position player in
  match evaluate new_game with
  | Evaluation.Game_over { winner = Some p } ->
      if Piece.equal p player then 1.0 else -1.0
  | Evaluation.Game_over { winner = None } -> 0.0
  | Evaluation.Illegal_move -> -10.0
  | Evaluation.Game_continues ->
      let possible_moves = available_moves new_game in
      let opponent_best_score =
        List.map possible_moves ~f:(fun pos ->
            let score =
              minimax ~position:pos ~player:(Piece.flip player) new_game
            in
            score)
        |> List.fold ~init:(-.Float.infinity) ~f:Float.max
      in
      -.opponent_best_score

let make_move ~(game : Game.t) ~(you_play : Piece.t) : Position.t =
  print_s [%message (you_play : Piece.t)];
  print_game game;
  let all_available = available_moves game in
  let best_move, _ =
    List.fold all_available
      ~init:(List.hd_exn all_available, -.Float.infinity)
      ~f:(fun (best_pos, best_score) position ->
        let score = minimax ~position ~player:you_play game in
        print_s [%message (position : Position.t) (score : float)];
        if Float.(score > best_score) then (position, score)
        else (best_pos, best_score))
  in
  best_move

(* let almost_win_for_x =
  init_game
    [
      ({ row = 0; column = 0 }, X);
      ({ row = 2; column = 0 }, O);
      ({ row = 1; column = 1 }, O);
      ({ row = 0; column = 2 }, X);
      ({ row = 0; column = 1 }, O);
      ({ row = 1; column = 2 }, O);
      ({ row = 2; column = 1 }, O);
    ] *)

let%expect_test "make move" =
  let move = make_move ~game:non_win ~you_play:Piece.X in
  print_s [%sexp (move : Position.t)];
  print_s [%message (evaluate non_win : Evaluation.t)];
  [%expect {|(((row 1) (column 1)))|}];
  return ()
