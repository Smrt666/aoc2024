let file =  open_in "input.txt"
let rec read_lines acc file = 
  try
    let line = input_line file in
    let xl = List.map int_of_string (String.split_on_char ' ' line) in
    let sgn = if List.hd xl > List.hd (List.tl xl) then 1 else -1 in
    let _ = print_endline (string_of_int sgn) in 
    let rec solve l = match l with
      | [] -> 1
      | _ :: [] -> 1
      | x :: y :: r -> if sgn * (x - y) > 0 && sgn * (x - y) <= 3 then let _ = print_endline (string_of_int (sgn * (x - y))) in solve (y :: r) else 0 in
    let r = solve xl in
    let _ = print_string "line result: " in
    let _ = print_endline (string_of_int r) in
    read_lines (acc + r) file
  with End_of_file -> (close_in file; acc)

let r = read_lines 0 file

let () = Printf.printf "Result:\n%d\n" r