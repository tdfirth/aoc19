open Core

let get_lines file = In_channel.read_lines file

let split line char = String.split line ~on:char
