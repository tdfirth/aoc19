open Core

let get_lines file = In_channel.read_lines file

let split line char = String.split line ~on:char

let printi i = Printf.printf "%d\n" i

let rec print_list = function
  | [] -> print_string "\n"
  | e :: l ->
      printi e;
      print_string " ";
      print_list l
