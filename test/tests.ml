open Aoc19

let run_test day assertion expectation =
  if assertion = expectation then
    Printf.printf "Test case for day %d succeeded.\n" day
  else Printf.printf "Test case for day %d failed.\n" day

let day_1_test =
  run_test 1 (Day1.fuel_required 12) 2;
  run_test 1 (Day1.fuel_required 14) 2;
  run_test 1 (Day1.fuel_required 1969) 654;
  run_test 1 (Day1.fuel_required 100756) 33583

let day_2_test =
  run_test 2 (Day2.eval [ 1; 0; 0; 0; 99 ]) [ 2; 0; 0; 0; 99 ];
  run_test 2 (Day2.eval [ 2; 3; 0; 3; 99 ]) [ 2; 3; 0; 6; 99 ];
  run_test 2 (Day2.eval [ 2; 4; 4; 5; 99; 0 ]) [ 2; 4; 4; 5; 99; 9801 ];
  run_test 2
    (Day2.eval [ 1; 1; 1; 4; 99; 5; 6; 0; 99 ])
    [ 30; 1; 1; 4; 2; 5; 6; 0; 99 ]
