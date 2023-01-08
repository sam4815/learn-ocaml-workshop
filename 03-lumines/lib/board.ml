open Base

type t =
  { board : Filled_square.t option array array
  ; height : int
  ; width : int
  }

let create ~height ~width =
  { board = Array.make_matrix ~dimx:width ~dimy:height None; height; width }
;;

let get t { Point.col; row } = t.board.(col).(row)
let set t { Point.col; row } value = t.board.(col).(row) <- value

let is_color t point color =
  match get t point with
  | None -> false
  | Some square -> Color.equal square.color color
;;

let is_square_color t ~bottom_left ~color =
  List.fold_left
    (Moving_piece.coords ~bottom_left)
    ~f:(fun valid point -> valid && (is_color t point color))
    ~init:true
;;

let to_sweep_square t ~bottom_left =
  List.iter
    (Moving_piece.coords ~bottom_left)
    ~f:(fun point ->
      match get t point with
      | None -> ()
      | Some square -> square.sweeper_state <- To_sweep
    )
;;

let mark_squares_that_are_sweepable t =
  (* TODO: at the end of this function, all filled_squares that are part of
     completed squares (i.e. four tiles in a square arrangement that are all of
     the same colors) should be in sweeper state [To_sweep], and all other
     squares should be [Unmarked].

     Note that, for example, a 2x3 rectangle of all the same color should also
     be marked by these criteria. *)
  List.iter (List.range 0 (((t.height-1) * t.width-1))) ~f:(fun i ->
    let point = { Point.row = (i/t.width); col = (i%(t.width-1)) } in
    match get t point with
    | None -> ()
    | Some square -> if is_square_color t ~bottom_left:point ~color:square.color then
        to_sweep_square t ~bottom_left:point
      else ()
  )
;;

let shift_col_down t { Point.row; col } =
  List.iter (List.range row (t.height - 1)) ~f:(fun i ->
    set t { Point.row = i; col = col } (get t { Point.row = i + 1; col = col})
  );
  set t { Point.row = t.height-1; col = col } None
;;

let remove_squares t =
  (* TODO: remove any squares marked as [Swept] from the board.  Gravity should
     be applied appropriately. This is the function that is called by the
     [Sweeper.t] to clear squares from the board.

     At the end of this function, we should call
     [mark_squares_that_are_sweepable] so that we ensure that we leave the board
     in a valid state.  *)
  List.iter (List.rev (List.range 0 (t.height * t.width))) ~f:(fun i ->
    let point = { Point.row = (i/t.width); col = (i % t.width) } in
    match get t point with
    | Some { sweeper_state = Swept; _ } -> shift_col_down t point
    | _ -> ()
  );
  mark_squares_that_are_sweepable t
;;

let is_empty t point =
  match get t point with
  | None -> true
  | Some _ -> false
;;

let is_square_empty t ~bottom_left =
  List.fold_left
    (Moving_piece.coords ~bottom_left)
    ~f:(fun valid point -> valid && (is_empty t point))
    ~init:true
;;

let is_square_bottom_empty t ~bottom_left =
  is_empty t bottom_left && is_empty t { Point.col = bottom_left.col + 1; row = bottom_left.row }
;;

let add_piece_and_apply_gravity t ~moving_piece:{ Moving_piece.top_left; top_right; bottom_left; bottom_right } ~col =
  (* TODO: insert (affix) the moving piece into the board, applying gravity
     appropriately. Make sure to leave the board in a valid state. *)
  let first_blocked_index =
    List.find
      (List.rev (List.range 0 (t.height - 1)))
      ~f:(fun i -> not (is_square_empty t ~bottom_left:{ Point.col = col; row = i})) in
  let moving_piece_index = match first_blocked_index with
  | None -> 0
  | Some i -> i + 1 in
  if moving_piece_index < t.height - 1 then (
    set t { col = col; row = moving_piece_index + 1 } (Some top_left);
    set t { col = col + 1; row = moving_piece_index + 1 } (Some top_right);
    set t { col = col; row = moving_piece_index } (Some bottom_left);
    set t { col = col + 1; row = moving_piece_index } (Some bottom_right);
    true
  ) else false
;;

(* Tests *)
let is_filled_with_color t ~row ~col color =
  match get t { Point.row; col } with
  | None -> false
  | Some square -> Color.equal color square.color
;;

let is_marked t ~row ~col =
  match get t { Point.row; col } with
  | None -> false
  | Some square ->
    Filled_square.Sweeper_state.equal
      square.Filled_square.sweeper_state
      Filled_square.Sweeper_state.To_sweep
;;

let test_piece =
  { Moving_piece.top_left = Filled_square.create Color.Orange
  ; top_right = Filled_square.create Color.White
  ; bottom_left = Filled_square.create Color.White
  ; bottom_right = Filled_square.create Color.White
  }
;;

let%test "Testing add_piece_and_apply_gravity add one..." =
  let t = create ~height:4 ~width:4 in
  add_piece_and_apply_gravity t ~moving_piece:test_piece ~col:0
  && is_filled_with_color t ~row:0 ~col:0 Color.White
  && is_filled_with_color t ~row:0 ~col:1 Color.White
  && is_filled_with_color t ~row:1 ~col:0 Color.Orange
  && is_filled_with_color t ~row:1 ~col:1 Color.White
;;

let%test "Testing add_piece_and_apply_gravity add many..." =
  let t = create ~height:4 ~width:4 in
  add_piece_and_apply_gravity t ~moving_piece:test_piece ~col:0
  && add_piece_and_apply_gravity t ~moving_piece:test_piece ~col:0
  && not (add_piece_and_apply_gravity t ~moving_piece:test_piece ~col:0)
;;

let test_removable_piece =
  { Moving_piece.top_left = Filled_square.create Color.White
  ; top_right = Filled_square.create Color.White
  ; bottom_left = Filled_square.create Color.White
  ; bottom_right = Filled_square.create Color.White
  }
;;

let%test "Testing mark_squares_that_are_sweepable..." =
  let t = create ~height:4 ~width:4 in
  assert (add_piece_and_apply_gravity t ~moving_piece:test_removable_piece ~col:0);
  assert (add_piece_and_apply_gravity t ~moving_piece:test_piece ~col:0);
  mark_squares_that_are_sweepable t;
  is_marked t ~row:0 ~col:0
  && is_marked t ~row:0 ~col:1
  && is_marked t ~row:1 ~col:0
  && is_marked t ~row:1 ~col:1
  && is_marked t ~row:2 ~col:0
  && is_marked t ~row:2 ~col:1
  && (not (is_marked t ~row:3 ~col:0))
  && not (is_marked t ~row:3 ~col:1)
;;

let sweep_board t =
  Array.iter t.board ~f:(fun row ->
      Array.iter row ~f:(fun square ->
          Option.iter square ~f:(fun square -> ignore (Filled_square.sweep square))))
;;

let%test "Testing Remove_squares..." =
  let t = create ~height:4 ~width:4 in
  assert (add_piece_and_apply_gravity t ~moving_piece:test_removable_piece ~col:0);
  assert (add_piece_and_apply_gravity t ~moving_piece:test_piece ~col:0);
  mark_squares_that_are_sweepable t;
  sweep_board t;
  remove_squares t;
  is_filled_with_color t ~row:0 ~col:0 Color.Orange
  && is_filled_with_color t ~row:0 ~col:1 Color.White
;;
