open Scaffold

module Direction : sig
  type t =
    | Facing_up
    | Facing_down
    | Facing_left
    | Facing_right
end

module Game_state : sig
  type t =
    | Playing
    | Won
    | Dead
end

module Frog : sig
  type t

  val direction : t -> Direction.t
  val position : t -> Position.t
end

module Non_frog_character : sig
  module Kind : sig
    type t =
      | Car1
      | Car2
      | Car3
      | Log1
      | Log2
      | Log3
  end

  type t

  val kind : t -> Kind.t
  val position : t -> Position.t
  val horizontal_speed : t -> int
end

module World : sig
  type t

  val frog : t -> Frog.t
  val nfcs : t -> Non_frog_character.t list
  val state : t -> Game_state.t
end

val random_element : 'a array -> 'a

val create_frog : unit -> Frog.t
val create_nfc : Non_frog_character.Kind.t -> int -> int -> Non_frog_character.t
val create_log : int -> int -> Non_frog_character.t
val create_car : int -> int -> Non_frog_character.t

val move_nfc : Non_frog_character.t -> Non_frog_character.t
val move_nfcs : Non_frog_character.t list -> Non_frog_character.t list
val is_position_inside_board : Position.t -> bool
val is_nfc_on_board : Non_frog_character.t -> bool
val is_frog_on_board : World.t -> bool

val add_nfcs : Non_frog_character.t list -> Non_frog_character.t list
val remove_stale_nfcs : Non_frog_character.t list -> Non_frog_character.t list
val generate_random_nfcs : unit -> Non_frog_character.t list

val push_nfc : Non_frog_character.t -> Non_frog_character.t
val push_nfcs : Non_frog_character.t list -> Non_frog_character.t list

val should_add_log : unit -> bool
val should_add_car : unit -> bool

val nfc_from_row : int -> Non_frog_character.t list -> Board.Row.t -> Non_frog_character.t list

val is_log : Non_frog_character.t -> bool
val is_car : Non_frog_character.t -> bool

val does_frog_overlap_nfc : Frog.t -> Non_frog_character.t -> bool

val logs_with_frog : World.t -> Non_frog_character.t list
val move_frog_with_log : Frog.t -> Non_frog_character.t -> Frog.t
val is_frog_on_log : World.t -> bool
val is_frog_on_car : World.t -> bool

val filter_river : int -> int list -> Board.Row.t -> int list
val river_indices : int list
val is_frog_in_river : World.t -> bool

val is_frog_dead : World.t -> bool

val calculate_position : Frog.t -> Key.t -> Position.t

val move_frog : World.t -> Key.t -> World.t

val get_frog_image : World.t -> Image.t
val get_nfc_image : Non_frog_character.t -> Image.t

val tick_frog : World.t -> World.t
val tick_nfcs : World.t -> World.t

val create : unit -> World.t
val tick : World.t -> World.t
val calculate_direction : Key.t -> Direction.t
val handle_input : World.t -> Key.t -> World.t
val handle_event : World.t -> Event.t -> World.t
val draw : World.t -> Display_list.t
val finished : World.t -> bool
