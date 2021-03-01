let read = ref read_line;
let rec input_valid prompt is_valid cast =
  let rec urs () =
    let s = !read () in
    if is_valid s then cast s
    else
      let () = print_endline "Réessayez!" in
      urs ()
  in
  print_endline prompt; urs ();
 
 ;
 let string_of_player = function 
  | H n when (1 <= n && n <= 4)-> "Joueur " ^ (string_of_int n) ^ " (humain)"
  | B n when (1 <= n && n <= 4)-> "Joueur " ^ (string_of_int n) ^ " (bot)"
  | _ -> failwith "Le joueur doit etre un nombre en 1 et 4"

;
let rec string_of_dominoes = function 
  |[] -> ""
  |D(x, y)::l -> (string_of_int x)^"-"^(string_of_int y)^" "^( string_of_dominoes l)
