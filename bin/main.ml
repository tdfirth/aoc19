open Aoc19

let () =
  Day2.printi
    (List.nth
       (Day2.eval (Day2.set_vals (Day2.load_program "inputs/day2") 12 2))
       0)
