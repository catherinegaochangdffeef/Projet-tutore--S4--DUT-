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

   let rec possible_dominoes lst cd =
   match lst with 
   |[]->[]
   |dlst::rstlst-> if (legal_adds dlst cd)!=[] then dlst::(possible_dominoes rstlst cd) else possible_dominoes rstlst cd



(* SECTION 2 : Sélection du coup à jouer et calcul du résultat *)

  let rec input_valid prompt is_valid cast =
    let rec urs () =
      let s = !read () in
      if is_valid s then cast s
      else
        let () = print_endline "Réessayez!" in urs ()
    in
    print_endline prompt; urs ();;

  
    let rec suppress c cd =
      match cd with
      |[]->[]
      |h::t -> if c=h || flip(c)=h then t else h::(suppress c t)
  (* input_move *)
  (* input_bot_move *)
  (* input_human_move *)

(* SECTION 3 : Gestion complète d'un coup, avec affichages et pioche éventuelle *)
  
  let string_of_player = function 
  | H n when (1 <= n && n <= 4)-> "Joueur " ^ (string_of_int n) ^ " (humain)"
  | B n when (1 <= n && n <= 4)-> "Joueur " ^ (string_of_int n) ^ " (bot)   "
  | _ -> failwith "Le joueur doit etre un nombre en 1 et 4"
  ;;

  (* take *)
  (* move *)

  let rec string_of_dominoes = function 
  |[] -> ""
  |D(x, y)::l -> String.trim((string_of_int x)^"-"^(string_of_int y)^" "^( string_of_dominoes l))
  ;;

(* SECTION 4 : Mise en place d'une partie *)

  let char_list_of_string str =
    let rec urs n l =
      if n < 0 then 
        l 
      else 
        urs (n-1) (str.[n]::l) 
    in urs (String.length str - 1) []
  ;;

  let players_of_string str =
    let rec urs n l =
      if n < 0 then 
        l 
      else 
        match str.[n] with
        |'B' -> urs (n-1) ([B (n+1)] @ l) 
        |'H' -> urs (n-1) ([H (n+1)] @ l) 
        | _ -> failwith "Le joueur ne peut etre qu'un Humain ou un Bot"
    in urs (String.length str - 1) []
  ;;
  
  let make_dominoes x =
      let rec urs l x = 
        function
        | 0 when x=0 -> (l @ [D(0,0)])
        | 0 -> urs (l @ [D(x,0)]) (x-1) (x-1)
        | y -> urs (l @ [D(x,y)]) x (y-1)
      in urs [] x x;;

  let get_hand_size = function
    | 2 -> 7
    | 3 -> 6
    | 4 -> 6
    | _ -> failwith "Entre 2 et 4 joueurs, please!"
  ;;
   
  (* make_state_list *)

(* SECTION 5 : Jeu proprement dit *)
  let string_of_chain = function
    | S (_, x, _) -> x
    | _ -> ""
  ;;

 let string_of_state (x, p)= string_of_player(p) ^ ":\t" ^ string_of_dominoes(x)
  ;;

  (* list_shuffle *)

  (* play *)
