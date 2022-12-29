open! Base

type t = { location : Position.t } [@@deriving sexp_of]

let location t = t.location

(* TODO: Implement [create].

   Make sure to inspect the mli to understand the signature of[create]. [create]
   will take in the height and width of the board area, as well as a list of
   locations where the apple cannot be generated, and create a [t] with a random
   location on the board.

   Hint: 
   - You can generate a random int up to [bound] via [Random.int bound].
   - You can pick a random element out of a list using [List.random_element_exn list]. 
*)
let create ~height ~width ~invalid_locations =
   let valid_locations =
      List.init height ~f:(fun row ->
         List.init width ~f:(fun col -> { Position.row; col }))
      |> List.concat
      |> List.filter ~f:(fun pos -> not (List.exists invalid_locations ~f:(fun location -> ([%compare.equal: Position.t] location pos))))
   in
   (* Stdio.printf !"%{sexp: Position.t list}\n%!" valid_locations; *)
   match List.length valid_locations with
   | 0 -> None
   | _ -> Some { location = List.random_element_exn valid_locations }
