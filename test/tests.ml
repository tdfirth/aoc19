open Aoc19

let run_test day assertion expectation =
  if assertion = expectation then
    Printf.printf "Test case %d = %d for day %d succeeded.\n" assertion
      expectation day
  else
    Printf.printf "Test case for day %d failed. Expected %d got %d\n" day
      assertion expectation

let day_1_test =
  run_test 1 (Day1.fuel_required 12) 2;
  run_test 1 (Day1.fuel_required 14) 2;
  run_test 1 (Day1.fuel_required 1969) 654;
  run_test 1 (Day1.fuel_required 100756) 33583
