let file =  open_in "input.txt"

let solve xl = 
  let sgn = if List.hd xl > List.hd (List.tl xl) then 1 else -1 in
  let rec solve l = match l with
    | [] -> 1
    | _ :: [] -> 1
    | x :: y :: r -> if sgn * (x - y) > 0 && sgn * (x - y) <= 3 then solve (y :: r) else 0 in
  solve xl

let rec remove_nth l n = match l with
  | [] -> []
  | x :: r -> if n = 0 then r else x :: remove_nth r (n - 1)

let rec read_lines acc file = 
  try
    let line = input_line file in
    let xl = List.map int_of_string (String.split_on_char ' ' line) in
    let rec try_all l n = if n <= List.length l then if solve (remove_nth l n) = 1 then 1 else try_all l (n + 1) else 0 in 
    let r = try_all xl 0 in
    print_endline (string_of_int (solve (remove_nth xl (List.length xl))));
    read_lines (acc + r) file
  with End_of_file -> (close_in file; acc)

let r = read_lines 0 file

let () = Printf.printf "Result:\n%d\n" r