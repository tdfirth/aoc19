(* Day2
 *)
open List

let load_program path =
  List.map int_of_string (Utils.split (hd (Utils.get_lines path)) ',')

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
    if op = 99 then p
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

let cart_prod l1 l2 =
  List.fold_left
    (fun acc1 ele1 ->
      List.fold_left (fun acc2 ele2 -> (ele1, ele2) :: acc2) acc1 l2)
    [] l1

let find_solution program goal =
  let ns = Core.Sequence.to_list (Core.Sequence.range 0 100) in
  let inputs = cart_prod ns ns in
  let compute (a, b) = nth (eval (set_vals program a b)) 0 in
  let result r = match r with a, b -> (100 * a) + b in
  let rec search i =
    if compute (hd i) = goal then result (hd i) else search (tl i)
  in
  search inputs
