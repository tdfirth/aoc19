let rec fuel_required mass =
  let fuel = (mass / 3) - 2 in
  if fuel < 0 then 0 else fuel + fuel_required fuel

let get_weights file = List.map int_of_string (Utils.get_lines file)

let total_fuel input =
  List.fold_left (fun x a -> x + fuel_required a) 0 (get_weights input)
