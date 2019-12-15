let fuel_required mass = (mass / 3) - 2

let get_weights file = List.map int_of_string (Utils.get_lines file)

let total_fuel input =
  List.fold_left (fun x a -> x + fuel_required a) 0 (get_weights input)
