(* 1. Find the intersections. 2D cartesian space, use 0, 0 as central port. 
 * Find all points each line goes through. Find common elements in two sets.
 * 2. Find the distances from the central port. 'Manhattan distance', which for (x, y) to (0, 0) is x + y.
 * 3. Return minimum... pretty obvious 
 *)

open Core

module Point = struct
  (* TODO Make this a record with a coordinate and an 'n'. *)
  module T = struct
    type t = { x : int; y : int; n : int } [@@deriving sexp_of]

    (* Only x and y determine if a point has been visited.
     * This does mean that if a point is visited twice we lose
     * the information about when that happened. However
     * for this problem we always care about the lowest n 
     * for that point so it's fine. *)
    let compare a b =
      let cmp_x = Int.compare a.x b.x in
      if cmp_x <> 0 then cmp_x else Int.compare a.y b.y

    let make x y n = { x; y; n }

    let pp p = Printf.printf "( %d, %d, %d )" p.x p.y p.n
  end

  include T
  include Comparator.Make (T)
end

let parse_step step =
  let dir = step.[0] in
  let mag = String.drop_prefix step 1 |> Int.of_string in
  match dir with
  | 'R' -> Point.make mag 0 0
  | 'L' -> Point.make (-mag) 0 0
  | 'U' -> Point.make 0 mag 0
  | 'D' -> Point.make 0 (-mag) 0
  | _ -> raise (Failure "Invalid direction.")

let visited steps =
  let rec outer current remaining visited =
    let rec inner start step visited =
      match step with
      | { Point.x = 0; Point.y = 0; _ } -> (start, visited)
      | { Point.x = a; Point.y = b; _ } -> (
          match start with
          | { Point.x; Point.y; Point.n } ->
              let inc s = if s = 0 then 0 else s / abs s in
              let inc_x = inc a in
              let inc_y = inc b in
              let move_to = Point.make (x + inc_x) (y + inc_y) (n + 1) in
              let next_step = Point.make (a - inc_x) (b - inc_y) 0 in
              (* Point.pp move_to; *)
              inner move_to next_step (Set.add visited move_to) )
    in
    match remaining with
    | [] -> visited
    | step :: rest ->
        let next, visited = inner current (parse_step step) visited in
        outer next rest visited
  in
  outer (Point.make 0 0 0) steps (Set.empty (module Point))

let compute_path p = Utils.split p ',' |> visited

let min l =
  let rec loop min tail =
    match tail with
    | [] -> min
    | x :: xs -> loop (if x < min then x else min) xs
  in
  loop (List.hd_exn l) (List.tl_exn l)

let find_nearest a b =
  (* Set.iter a ~f:Point.pp; *)
  (* Set.iter b ~f:Point.pp; *)
  let intersections = Set.inter a b |> Set.to_list in
  let delay p =
    let cmp a b =
      let c = Point.compare a b in
      if c = 0 then false else true
    in
    let find x p =
      Set.find x ~f:(fun other -> cmp p other) |> Option.value_exn
    in
    (find a p).n + (find b p).n
  in
  let delays = List.map ~f:delay intersections in
  (* List.iter ~f:(Printf.printf "%d ") delays; *)
  min delays
