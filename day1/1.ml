let file =  open_in "input.txt"
let rec read_lines acl acr file = 
  try
    let line = input_line file in
    let l_r = String.split_on_char ' ' line in
    let l = List.nth l_r 0 in
    let r = List.nth l_r (List.length l_r - 1) in
    read_lines (int_of_string l::acl) (int_of_string r::acr) file
  with End_of_file -> (acl, acr)

let (l, r) = read_lines [] [] file
let sl, sr = List.sort compare l, List.sort compare r

let result = List.fold_left2 (fun acc a b -> abs (a - b) + acc) 0 sl sr

let () = Printf.printf "Result:\n%d\n" result