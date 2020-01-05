(* 1. Find the intersections. 2D cartesian space, use 0, 0 as central port. 
 * Find all points each line goes through. Find common elements in two sets.
 * 2. Find the distances from the central port. 'Manhattan distance', which for (x, y) to (0, 0) is x + y.
 * 3. Return minimum... pretty obvious 
 *)

open Core

module Point = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp_of]

    let pp (x, y) = Printf.printf "( %d, %d )" x y
  end

  include T
  include Comparator.Make (T)
end

let parse_step step =
  let dir = step.[0] in
  let mag = String.drop_prefix step 1 |> Int.of_string in
  match dir with
  | 'R' -> (mag, 0)
  | 'L' -> (-mag, 0)
  | 'U' -> (0, mag)
  | 'D' -> (0, -mag)
  | _ -> raise (Failure "Invalid direction.")

let visited steps =
  let inc s = if s = 0 then 0 else s / abs s in
  let rec outer current_position remaining all_points n =
    let rec inner start_position step positions_visited n =
      match step with
      | 0, 0 -> (start_position, positions_visited, n)
      | a, b -> (
          match start_position with
          | x, y ->
              let inc_x = inc a in
              let inc_y = inc b in
              let move_to = (x + inc_x, y + inc_y) in
              let next_step = (a - inc_x, b - inc_y) in
              let updated_n = n + 1 in
              let updated_positions =
                match
                  Map.add positions_visited ~key:move_to ~data:updated_n
                with
                | `Ok m -> m
                | `Duplicate -> positions_visited
              in
              inner move_to next_step updated_positions updated_n )
    in
    match remaining with
    | [] -> all_points
    | step :: rest ->
        let next, visited, new_n =
          inner current_position (parse_step step) all_points n
        in
        outer next rest visited new_n
  in
  outer (0, 0) steps (Map.empty (module Point)) 0

let compute_path p = Utils.split p ',' |> visited

let min l =
  let rec loop min tail =
    match tail with
    | [] -> min
    | x :: xs -> loop (if x < min then x else min) xs
  in
  loop (List.hd_exn l) (List.tl_exn l)

let find_nearest a b =
  let intersections =
    let a_visited = Set.of_list (module Point) (Map.keys a) in
    let b_visited = Set.of_list (module Point) (Map.keys b) in
    Set.inter a_visited b_visited |> Set.to_list
  in
  let delay p = Map.find_exn a p + Map.find_exn b p in
  let delays = List.map ~f:delay intersections in
  min delays
