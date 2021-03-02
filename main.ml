let read = ref read_line;;
  
type domino = D of int * int;;

type chain = E | S of int * string * int;;

type player = H of int | B of int;;
 
(* SECTION 1 : Gestion des coups légaux *)
  let flip (D (x,y)) = 
    D (y,x)
  ;;

  let append (D (d1,d2), chain, char) = 
    let domino = ((string_of_int d1)^"-"^ (string_of_int d2)) in
      match (D (d1,d2), chain, char) with
      | (D (d1,d2), E, _) -> S (d1, domino, d2)
      | (D (d1,_), S (_, str, fin), '<') -> S (d1, domino ^ " " ^ str , fin)
      | (D (_,d2), S (debut, str, _), '>') -> S (debut, str ^ " " ^ domino , d2)
      | (_,_,_) -> E
    ;;

  let legal_adds c cd = 
  match cd with 
  |E->(append(c,E,'>'))::[]
  |S(x,y,z)->match c with 
      |D(a,b) when(b=x && a=z)&& a!=b ->append(c,cd,'<')::append(c,cd,'>')::[]
      |D(a,b) when (a=x && b=z)&&a!=b -> append(flip c, cd,'<')::append(flip c, cd,'>')::[]
      |D(a,b) when b=x -> append(c,cd,'<')::[]
      |D(a,b) when a=x -> append(flip c,cd,'<')::[]
      |D(a,b) when a = z -> append(c,cd,'>')::[]
      |D(a,b) when b=z -> append(flip c, cd,'>')::[]
      |_->[];;

   let rec possible_dominos lst cd =
   match lst with 
   |[]->[]
   |dlst::rstlst-> if (legal_adds dlst cd)!=[] then dlst::(possible_dominos rstlst cd) else possible_dominos rstlst cd



(* SECTION 2 : Sélection du coup à jouer et calcul du résultat *)

  let rec input_valid prompt is_valid cast =
    let rec urs () =
      let s = !read () in
      if is_valid s then cast s
      else
        let () = print_endline "Réessayez!" in urs ()
    in
    print_endline prompt; urs ();;

  (* suppress *)
  (* input_move *)
  (* input_bot_move *)
  (* input_human_move *)

(* SECTION 3 : Gestion complète d'un coup, avec affichages et pioche éventuelle *)
  
  let string_of_player = function 
    | H n when (1 <= n && n <= 4)-> "Joueur " ^ (string_of_int n) ^ " (humain)"
    | B n when (1 <= n && n <= 4)-> "Joueur " ^ (string_of_int n) ^ " (bot)"
    | _ -> failwith "Le joueur doit etre un nombre en 1 et 4"
    ;;

  (* take *)
  (* move *)

  let rec string_of_dominoes = function 
    |[] -> ""
    |D(x, y)::l -> (string_of_int x)^"-"^(string_of_int y)^" "^( string_of_dominoes l)
  ;;


(* SECTION 4 : Mise en place d'une partie *)
  (* make_dominoes *)
  (* char_list_of_string *)
  (* players_of_string *)
  (* get_hand_size *)
  (* make_state_list *)

(* SECTION 5 : Jeu proprement dit *)
  let string_of_chain = function
    | S (_, x, _) -> x
    | _ -> ""
  ;;

  let string_of_state = function
    | ([], p) -> string_of_player(p) ^ " " 
    | (x, p) -> string_of_player(p) ^ " " ^ string_of_dominoes(x)
  ;;

  (* list_shuffle *)

  (* play *)