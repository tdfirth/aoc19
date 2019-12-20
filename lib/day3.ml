(* 1. Find the intersections. 2D cartesian space, use 0, 0 as central port. 
 * Find all points each line goes through. Find common elements in two sets.
 * 2. Find the distances from the central port. 'Manhattan distance', which for (x, y) to (0, 0) is x + y.
 * 3. Return minimum... pretty obvious 
 *)

open Core

module Point = struct
  (* TODO Make this a record with a coordinate and an 'n'. *)
  module T = struct
    type t = int * int [@@deriving compare, sexp_of]

    let pp (x, y) = Format.printf "( %d, %d )" x y
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

let find_coords_betweeen start step =
  let rec loop (a, b) step visited =
    match step with
    | 0, 0 -> visited
    | _ ->
        let next, remaining =
          match step with
          | x, 0 ->
              if x > 0 then ((a + 1, b), (x - 1, 0))
              else ((a - 1, b), (x + 1, 0))
          | 0, y ->
              if y > 0 then ((a, b + 1), (0, y - 1))
              else ((a, b - 1), (0, y + 1))
          | _, _ -> raise (Failure "Invalid step.")
        in
        loop next remaining (Set.add visited next)
  in
  loop start step (Set.empty (module Point))

let coords_visited steps =
  let rec loop remaining coords last =
    match remaining with
    | [] -> coords
    | step :: tail -> (
        let s = parse_step step in
        match s with
        | x, y -> (
            match last with
            | a, b ->
                let c = (x + a, y + b) in
                loop tail (Set.union coords (find_coords_betweeen last s)) c ) )
  in
  loop steps (Set.empty (module Point)) (0, 0)

let compute_path p = Utils.split p ',' |> coords_visited

let min l =
  let rec loop min tail =
    match tail with
    | [] -> min
    | x :: xs -> loop (if x < min then x else min) xs
  in
  loop (List.hd_exn l) (List.tl_exn l)

let find_nearest a b =
  let intersections = Set.inter a b in
  let manhattan_dist (x, y) = Int.abs x + Int.abs y in
  let distances = Set.to_list intersections |> List.map ~f:manhattan_dist in
  min distances
