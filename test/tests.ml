open Aoc19

module Test (M : sig
  type t

  val day : int

  val compare : t -> t -> int

  val to_string : t -> string
end) =
struct
  let run msg assertion expectation =
    Printf.printf "Day %d: Running test %s..." M.day msg;
    let c = M.compare assertion expectation in
    if c = 0 then Printf.printf "OK\n"
    else
      Printf.printf "FAIL\n Expected %s but got %s.\n" (M.to_string expectation)
        (M.to_string assertion)
end

module TestDay1 = Test (struct
  let day = 1

  include Base.Int
end)

let day_1_test =
  (* Updated for part 2 *)
  TestDay1.run "12 gives 2" (Day1.fuel_required 12) 2;
  TestDay1.run "14 gives 2" (Day1.fuel_required 14) 2;
  TestDay1.run "1969 gives 966" (Day1.fuel_required 1969) 966;
  TestDay1.run "100756 gives 50346" (Day1.fuel_required 100756) 50346

module TestDay2 = Test (struct
  type t = int list

  let day = 2

  let compare x y =
    List.fold_left2
      (fun a b c -> if b = c then a && true else a && false)
      true x y
    |> function
    | true -> 0
    | false -> 1

  let to_string x =
    String.concat ", " (List.map string_of_int x) |> Printf.sprintf "[%s]"
end)

let day_2_test =
  TestDay2.run "case 1" (Day2.eval [ 1; 0; 0; 0; 99 ]) [ 2; 0; 0; 0; 99 ];
  TestDay2.run "case 2" (Day2.eval [ 2; 3; 0; 3; 99 ]) [ 2; 3; 0; 6; 99 ];
  TestDay2.run "case 3"
    (Day2.eval [ 2; 4; 4; 5; 99; 0 ])
    [ 2; 4; 4; 5; 99; 9801 ];
  TestDay2.run "case 4"
    (Day2.eval [ 1; 1; 1; 4; 99; 5; 6; 0; 99 ])
    [ 30; 1; 1; 4; 2; 5; 6; 0; 99 ]

module TestDay3 = Test (struct
  let day = 3

  include Base.Int
end)

let day_3_test =
  let one =
    Day3.find_nearest
      (Day3.compute_path "R75,D30,R83,U83,L12,D49,R71,U7,L72")
      (Day3.compute_path "U62,R66,U55,R34,D71,R55,D58,R83")
  in
  TestDay3.run "case 1" one 610;
  let two =
    Day3.find_nearest
      (Day3.compute_path "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51")
      (Day3.compute_path "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
  in
  TestDay3.run "case 2" two 410
