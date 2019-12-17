open List

let load_program path =
  List.map int_of_string (Utils.split (hd (Utils.get_lines path)) ',')

let rec print_list = function
  | [] -> print_string "\n"
  | e :: l ->
      print_int e;
      print_string " ";
      print_list l

let printi i = Printf.printf "%d\n" i

let split n xs =
  let rec loop acc n xs =
    match (n, xs) with
    | 0, xs -> (acc, xs)
    | _, [] -> (acc, [])
    | n, x :: xs -> loop (acc @ [ x ]) (n - 1) xs
  in
  loop [] n xs

(* Horrendously inefficient... but meh *)
let update l i x =
  let head, tail = split i l in
  head @ [ x ] @ tl tail

let eval program =
  let rec loop p i =
    let op = nth p i in
    (* Result in address 0 *)
    if op = 99 then nth p 0
    else
      let a = nth p (i + 1) in
      let b = nth p (i + 2) in
      let out = nth p (i + 3) in
      match op with
      | 1 -> loop (update p out (nth p a + nth p b)) (i + 4)
      | 2 -> loop (update p out (nth p a * nth p b)) (i + 4)
      | _ -> raise (Invalid_argument "Invalid program.")
  in
  loop program 0

let set_vals l a b = update (update l 1 a) 2 b
