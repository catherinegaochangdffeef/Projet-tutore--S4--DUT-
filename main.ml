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
      | (D (d1,_), S (_, str, rightchain), '<') -> S (d1, domino ^ " " ^ str , rightchain)
      | (D (_,d2), S (leftchain, str, _), '>') -> S (leftchain, str ^ " " ^ domino , d2)
      | (_,_,_) -> E
    ;;

  let legal_adds c cd = 
  match cd with 
  |E->(append(c,E,'>'))::[]
  |S(x,y,z)->match c with 
    |D(a,b) when (b=x && a=z) && a!=b -> append(c,cd,'<') :: append(c,cd,'>') ::[]
    |D(a,b) when (a=x && b=z) && a!=b -> append(flip c, cd,'<') :: append(flip c, cd,'>') ::[]
    |D(a,b) when b = x -> append(c,cd,'<')::[]
    |D(a,b) when a = x -> append(flip c,cd,'<')::[]
    |D(a,b) when a = z -> append(c,cd,'>')::[]
    |D(a,b) when b = z -> append(flip c, cd,'>')::[]
    |_ -> []
  ;;

  let rec possible_dominoes lst cd =
    match lst with 
    |[]->[]
    |dlst::rstlst-> if (legal_adds dlst cd)!=[] then dlst::(possible_dominoes rstlst cd) else possible_dominoes rstlst cd
;;


(* SECTION 2 : Sélection du coup à jouer et calcul du résultat *)

  let rec input_valid prompt is_valid cast =
    let rec urs () =
      let s = !read () in
      if is_valid s then cast s
      else
        let () = print_endline "Réessayez!" in urs ()
    in
    print_endline prompt; urs ();;

  (* TODO verifier les parenthèses et l'applications des fonctions *)
    let rec suppress c cd =
      match cd with
      |[]->[]
      |h::t -> if c = h || flip c = h then t else h :: suppress c t
  ;;
  (* TODO déplacer input_move à la fin pour tester sur VS les fonctions (il ne connait pas string_of_dominoes par ex qui est définie plus loin) *)
  let input_move select_domino select_end chain lst = 
    (* TODO faire un match*)
    if possible_dominoes lst chain = [] then 

      None 
    else 
      let domino = select_domino(possible_dominoes lst chain) in (*selectionner une possibilité parmi tous les possibilités possibles *)
        if List.length(legal_adds(domino) chain) = 1 then (*si la possibilité de chaine S est 1 *)
          let () = print_endline ("Coup forcé : " ^ string_of_dominoes [domino]) in 
            Some(suppress domino lst,List.nth(legal_adds(domino) (chain)) 0)
        else
        (* TODO variables intermédiares + espaces pour la lisibilité (formatteur de code à utiliser) *)
          match select_end(List.nth(legal_adds(domino)(chain))0)(List.nth(legal_adds(domino)(chain))1) with
          | S(a,b,c) -> Some(suppress domino lst,S(a,b,c))
          | _-> None             (* c'est pas obligatoire mais juste pour être sur *)
      ;;
  (* input_bot_move *)
  (* input_human_move *)

(* SECTION 3 : Gestion complète d'un coup, avec affichages et pioche éventuelle *)
  
  let string_of_player = function 
  | H n when (1 <= n && n <= 4)-> "Joueur " ^ (string_of_int n) ^ " (humain)"
  | B n when (1 <= n && n <= 4)-> "Joueur " ^ (string_of_int n) ^ " (bot)   "
  | _ -> failwith "Le joueur doit etre un nombre en 1 et 4"
  ;;

  let rec take l1 n l2 = 
    match (l1, n, l2) with 
    |(l1, n, []) -> (l1, [])
    |(l1, 0, l2) -> (l1, l2)
    |(l1, n, (D(x,y))::l) -> take ((D(x,y))::l1) (n-1) l
  ;;
  
  (* move *)

  let rec string_of_dominoes = function 
  |[] -> "" (* TODO sprintf pour les concaténations partout *)
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
    let rec urs l count = function
      | []                 -> l
      | x::list when x='B' -> urs (l @ [B (count+1)]) (count+1) list
      | _::list            -> urs (l @ [H (count+1)]) (count+1) list
    in urs [] 0 (char_list_of_string str);;
  
  let make_dominoes x =
    let rec urs l x = function
      | 0 when x=0 -> (l @ [D(0,0)])
      | 0 -> urs (l @ [D(x,0)]) (x-1) (x-1)
      | y -> urs (l @ [D(x,y)]) x (y-1)
    in urs [] x x
  ;;

  let get_hand_size = function
    | 2 -> 7
    | 3 -> 6
    | 4 -> 6
    | _ -> failwith "Entre 2 et 4 joueurs, please!"
  ;;

  let make_state_list string dominoes_list =
    let players_list = players_of_string string in 
    let playercount = List.length players_list in
    let number_domino_init = get_hand_size playercount in
      let rec urs l1 nb_domino_init l2 iter_player result =
        match (take l1 nb_domino_init l2) with
        | (l, stack) when iter_player = playercount -> (l @ stack, result)
        | (l, stack) -> urs [] nb_domino_init stack (iter_player+1) (result @ [(l, (List.nth players_list iter_player))] )
      in urs [] number_domino_init dominoes_list 0 []
;;

(* SECTION 5 : Jeu proprement dit *)
  let string_of_chain = function
    | S (_, x, _) -> x
    | _ -> ""
  ;;

  let string_of_state (x, p)= string_of_player(p) ^ ":\t" ^ string_of_dominoes(x)
  ;;
  
 let list_shuffle ls =
    let x = List.map (function y -> (Random.bits (), y)) ls in
      let comparList = List.sort compare x in
        List.map snd comparList
 ;;       

  (* play *)