let read = ref read_line;
let rec input_valid prompt is_valid cast =
  let rec urs () =
    let s = !read () in
    if is_valid s then cast s
    else
      let () = print_endline "Réessayez!" in
      urs ()
  in
  print_endline prompt; urs ()
  ;;
  
 type domino = D of int * int;;

type chain = E | S of int * string * int;;

type player = H of int | B of int;;

let read = ref read_line;;
 
let flip (D (x,y)) = 
  D (y,x)
;;

let append (D (d1,d2), chain, char) = 
let concaten = ((string_of_int d1)^"-"^ (string_of_int d2)) in
  match (D (d1,d2), chain, char) with
  | (D (d1,d2), E, _) -> S (d1, concaten, d2)
  | (D (d1,_), S (_, str, fin), '<') -> S (d1, concaten ^ " " ^ str , fin)
  | (D (_,d2), S (debut, str, _), '>') -> S (debut, str ^ " " ^ concaten , d2)
  | (_,_,_) -> E
;;
  
 let string_of_player = function 
  | H n when (1 <= n && n <= 4)-> "Joueur " ^ (string_of_int n) ^ " (humain)"
  | B n when (1 <= n && n <= 4)-> "Joueur " ^ (string_of_int n) ^ " (bot)"
  | _ -> failwith "Le joueur doit etre un nombre en 1 et 4"
;; 

let rec string_of_dominoes = function 
  |[] -> ""
  |D(x, y)::l -> (string_of_int x)^"-"^(string_of_int y)^" "^( string_of_dominoes l)
;;

let string_of_state = function
  | ([], _) -> ""
  | (x, H n) -> string_of_player(H n) ^ " " ^ string_of_dominoes(x)
  | (x, B n) -> string_of_player(B n) ^ " " ^ string_of_dominoes(x)
 ;;
 
 let string_of_chain = function
  | S (_, x, _) -> x
  | _ -> ""
 ;;
 

