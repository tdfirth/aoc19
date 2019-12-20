open Aoc19

let day2 () =
  let program = Day2.load_program "inputs/day2" in
  let r = Day2.find_solution program 19690720 in
  Printf.printf "%d" r

let day3 () =
  let paths = Utils.get_lines "inputs/day3" in
  let dist =
    Day3.find_nearest
      (List.nth paths 0 |> Day3.compute_path)
      (List.nth paths 1 |> Day3.compute_path)
  in
  Printf.printf "%d" dist

let () = day3 ()
