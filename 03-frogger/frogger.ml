open Base
open Scaffold

module Direction = struct
  type t =
    | Facing_up
    | Facing_down
    | Facing_left
    | Facing_right
end

module Game_state = struct
  type t =
    | Playing
    | Won
    | Dead
end

module Frog = struct
  type t =
    { position : Position.t;
      direction: Direction.t
    } [@@deriving fields]

  let create = Fields.create
end

module Non_frog_character = struct
  module Kind = struct
    type t =
      | Car1
      | Car2
      | Car3
      | Log1
      | Log2
      | Log3
  end

  type t =
    {
      kind : Kind.t;
      position : Position.t;
      horizontal_speed : int
    } [@@deriving fields]
  
  let create = Fields.create
end

module World = struct
  type t =
    { frog  : Frog.t;
      nfcs : Non_frog_character.t list;
      state : Game_state.t;
    } [@@deriving fields]

  let create = Fields.create
end

(* Initialize Random module *)
let _ = Random.self_init()

let random_element (arr) =
  let n = Random.int (Array.length arr) in
  Array.get arr n
;;

let create_frog () =
  Frog.create ~position:{ x = Board.num_cols / 2; y = 0; } ~direction:Facing_up
;;

let create_nfc (kind : Non_frog_character.Kind.t) (row : Int.t) (speed : Int.t) =
  Non_frog_character.create
    ~kind:kind
    ~position:{ x = (if speed > 0 then 0 else Board.num_cols - 1); y = row; }
    ~horizontal_speed:speed
;;

let logs = [| Non_frog_character.Kind.Log1; Non_frog_character.Kind.Log2; Non_frog_character.Kind.Log3 |]
let create_log (row : Int.t) (speed : Int.t) = create_nfc (random_element logs) row speed

let cars = [| Non_frog_character.Kind.Car1; Non_frog_character.Kind.Car2; Non_frog_character.Kind.Car3 |]
let create_car (row : Int.t) (speed : Int.t) = create_nfc (random_element cars) row speed

let move_nfc (nfc : Non_frog_character.t) =
  { nfc with position = { nfc.position with x = nfc.position.x + nfc.horizontal_speed }}

let move_nfcs (nfcs : Non_frog_character.t list) = List.map nfcs ~f:move_nfc

let is_position_inside_board (position : Position.t) =
  position.x >= 0 && position.x < Board.num_cols

let is_nfc_on_board (nfc : Non_frog_character.t) = is_position_inside_board nfc.position

let is_frog_on_board (world : World.t) = is_position_inside_board world.frog.position

let remove_stale_nfcs (nfcs : Non_frog_character.t list) = List.filter nfcs ~f:is_nfc_on_board

let should_add_log () = Random.int 10 > 5

let should_add_car () = Random.int 10 > 6

let nfc_from_row (idx : Int.t) (nfcs : Non_frog_character.t list) (row : Board.Row.t) =
  match row with
  | River -> if should_add_log () then [create_log idx (if idx%2 = 0 then 1 else -1)] @ nfcs else nfcs
  | Road -> if should_add_car () then [create_car idx (if idx%2 = 0 then 1 else -1)] @ nfcs else nfcs
  | Safe_strip -> nfcs

let add_nfcs (nfcs : Non_frog_character.t list) = nfcs @ List.foldi Board.rows ~f:nfc_from_row ~init:[]

let push_nfc (nfc : Non_frog_character.t) =
  { nfc with position = { nfc.position with x = Random.int (Board.num_cols - 1) } }

let push_nfcs (nfcs : Non_frog_character.t list) = List.map nfcs ~f:push_nfc

let generate_random_nfcs () =
  push_nfcs (add_nfcs (add_nfcs (add_nfcs [])))

let create () =
  World.create
    ~frog:(create_frog ())
    ~nfcs:(generate_random_nfcs ())
    ~state:Playing
;;

let is_log (nfc : Non_frog_character.t) =
  match nfc with
  | { kind = Log1; _ }
  | { kind = Log2; _ }
  | { kind = Log3; _ } -> true
  | _ -> false
;;

let is_car (nfc : Non_frog_character.t) =
  match nfc with
  | { kind = Car1; _ }
  | { kind = Car2; _ }
  | { kind = Car3; _ } -> true
  | _ -> false
;;

let does_frog_overlap_nfc (frog : Frog.t) (nfc : Non_frog_character.t) =
  nfc.position.x = frog.position.x && nfc.position.y = frog.position.y

let logs_with_frog (world : World.t) =
  List.filter world.nfcs ~f:(fun nfc -> is_log nfc && does_frog_overlap_nfc world.frog nfc)

let is_frog_on_log (world : World.t) =
  List.exists world.nfcs ~f:(fun nfc -> is_log nfc && does_frog_overlap_nfc world.frog nfc)

let is_frog_on_car (world : World.t) =
  List.exists world.nfcs ~f:(fun nfc -> is_car nfc && does_frog_overlap_nfc world.frog nfc)

let filter_river (idx : Int.t) (indices : Int.t list) (row : Board.Row.t) =
  match row with
  | River -> indices @ [idx]
  | _ -> indices

let river_indices = List.foldi Board.rows ~f:filter_river ~init:[]

let is_frog_in_river (world : World.t) =
  List.exists river_indices ~f:(fun el -> el = world.frog.position.y) && not (is_frog_on_log world)

let is_frog_dead (world : World.t) =
  is_frog_in_river world || is_frog_on_car world || not (is_frog_on_board world)

let finished (world : World.t) =
  world.frog.position.y = (List.length Board.rows - 1)
;;

let move_frog_with_log (frog : Frog.t) (log : Non_frog_character.t) =
  { frog with position = { frog.position with x = frog.position.x + log.horizontal_speed }}

let calculate_position (frog : Frog.t) (key : Key.t) =
  match key with
  | Arrow_up -> { frog.position with y = min (frog.position.y + 1) ((List.length Board.rows) - 1) }
  | Arrow_down -> { frog.position with y = max (frog.position.y - 1) 0 }
  | Arrow_left -> { frog.position with x = max (frog.position.x - 1) 0 }
  | Arrow_right -> { frog.position with x = min (frog.position.x + 1) (Board.num_cols - 1) }
  | _ -> frog.position

let calculate_direction (key : Key.t) =
  match key with
  | Arrow_up -> Direction.Facing_up
  | Arrow_down -> Direction.Facing_down
  | Arrow_left -> Direction.Facing_left
  | Arrow_right -> Direction.Facing_right
  | _ -> Direction.Facing_up

let move_frog (world : World.t) (key : Key.t) =
  { world with frog = { position = calculate_position world.frog key; direction = calculate_direction key } }

let get_frog_image (world : World.t) =
  match world with
  | { state = Dead; _ } -> Image.Skull_and_crossbones
  | { state = Won; _ } -> Image.Confetti
  | { frog = { direction = Facing_up; _ }; _ } -> Image.Frog_up
  | { frog = { direction = Facing_down; _ }; _ } -> Image.Frog_down
  | { frog = { direction = Facing_left; _ }; _ } -> Image.Frog_left
  | { frog = { direction = Facing_right; _ }; _ } -> Image.Frog_right

let get_nfc_image (nfc : Non_frog_character.t) =
  match nfc with
  | { kind = Log1; _ } -> Image.Log1
  | { kind = Log2; _ } -> Image.Log2
  | { kind = Log3; _ } -> Image.Log3
  | { kind = Car1; _ } -> if nfc.horizontal_speed > 0 then Image.Car1_right else Image.Car1_left
  | { kind = Car2; _ } -> if nfc.horizontal_speed > 0 then Image.Car2_right else Image.Car2_left
  | { kind = Car3; _ } -> if nfc.horizontal_speed > 0 then Image.Car3_right else Image.Car3_left

let tick_frog (world : World.t) =
  let logs = logs_with_frog world in
  match logs with
  | log :: _ -> { world with frog = move_frog_with_log world.frog log }
  | [] -> world

let tick_nfcs (world : World.t) =
  { world with nfcs = add_nfcs (remove_stale_nfcs (move_nfcs world.nfcs)) }

let tick (world : World.t) =
  let updated_world = tick_nfcs (tick_frog world) in
  match world with
  | { state = Dead; _ }
  | { state = Won; _ } -> world
  | _ -> { updated_world with state = if is_frog_dead updated_world then Dead else Playing }
;;

let handle_input (world : World.t) (key : Key.t) =
  let updated_world = move_frog world key in
  match world, key with
  | _, R -> create ()
  | { state = Dead; _ }, _
  | { state = Won; _ }, _ -> world
  | _ -> { updated_world with state = if is_frog_dead updated_world then Dead else if finished updated_world then Won else Playing }
;;

let draw (world : World.t) =
  List.map world.nfcs ~f:(fun nfc -> (get_nfc_image nfc, nfc.position))
  @ [(get_frog_image world, world.frog.position)]
;;

let handle_event (world : World.t) (event : Event.t) =
  match event with
  | Tick -> tick world
  | Keypress key -> handle_input world key
;;
