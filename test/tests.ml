open Aoc19

let run_test day assertion expectation =
  if assertion = expectation then
    Printf.printf "Test case for day %d succeeded.\n" day
  else Printf.printf "Test case for day %d failed.\n" day

let day_1_test =
  (* Updated for part 2 *)
  run_test 1 (Day1.fuel_required 12) 2;
  run_test 1 (Day1.fuel_required 14) 2;
  run_test 1 (Day1.fuel_required 1969) 966;
  run_test 1 (Day1.fuel_required 100756) 50346

let day_2_test =
  run_test 2 (Day2.eval [ 1; 0; 0; 0; 99 ]) [ 2; 0; 0; 0; 99 ];
  run_test 2 (Day2.eval [ 2; 3; 0; 3; 99 ]) [ 2; 3; 0; 6; 99 ];
  run_test 2 (Day2.eval [ 2; 4; 4; 5; 99; 0 ]) [ 2; 4; 4; 5; 99; 9801 ];
  run_test 2
    (Day2.eval [ 1; 1; 1; 4; 99; 5; 6; 0; 99 ])
    [ 30; 1; 1; 4; 2; 5; 6; 0; 99 ]

let day_3_test =
  let one =
    Day3.find_nearest
      (Day3.compute_path "R75,D30,R83,U83,L12,D49,R71,U7,L72")
      (Day3.compute_path "U62,R66,U55,R34,D71,R55,D58,R83")
  in
  run_test 3 one 159;
  let two =
    Day3.find_nearest
      (Day3.compute_path "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51")
      (Day3.compute_path "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
  in
  run_test 3 two 135
