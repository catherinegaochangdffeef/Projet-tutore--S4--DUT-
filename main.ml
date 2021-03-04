let read = ref read_line;;

type domino = D of int * int;;

type chain = E | S of int * string * int;;

type player = H of int | B of int;;

(* SECTION 1 : Gestion des coups légaux *)
  let rec string_of_dominoes = function 
  | []                    -> ""
  | D(x,y)::l when l = [] -> Printf.sprintf "%d-%d"    x y
  | D(x,y)::l             -> Printf.sprintf "%d-%d %s" x y (string_of_dominoes l)
  ;;


  let flip (D (x,y)) = 
    D (y,x)
  ;;

  let append (domino, chain, char) = 
    let domino_str = string_of_dominoes [domino] in
      match (domino, chain, char) with
      | (D (d1,d2), E                ,  _ ) -> S (d1  , domino_str, d2)
      | (D (d1,_) , S (_, str, rint) , '<') -> S (d1  , domino_str ^ " " ^ str , rint)
      | (D (_,d2) , S (lint, str, _) , '>') -> S (lint, str ^ " " ^ domino_str , d2)
      | _                            -> E
  ;;

  let legal_adds c cd = 
    match cd with 
    |E->(append(c,E,'>'))::[]
    |S(x,y,z)->match c with 
      |D(a,b) when (b=x && a=z) && a!=b -> append (c     , cd, '<')::append (c     , cd, '>')::[]
      |D(a,b) when (a=x && b=z) && a!=b -> append (flip c, cd, '<')::append (flip c, cd, '>')::[]
      |D(a,b) when b = x                -> append (c     , cd, '<')::[]
      |D(a,b) when a = x                -> append (flip c, cd, '<')::[]
      |D(a,b) when a = z                -> append (c     , cd, '>')::[]
      |D(a,b) when b = z                -> append (flip c, cd, '>')::[]
      |_ -> []
  ;;

  let rec possible_dominoes lst cd =
    match lst with 
    | []           -> []
    | dlst::rstlst -> if legal_adds dlst cd != [] then 
                        dlst::possible_dominoes rstlst cd 
                      else
                        possible_dominoes rstlst cd
  ;;


(* SECTION 2 : Sélection du coup à jouer et calcul du résultat *)

  let rec input_valid prompt is_valid cast =
    let rec urs () =
      let s = !read () in
        if is_valid s 
          then cast s
        else
          let () = print_endline "Réessayez!" in urs () in
            print_endline prompt; urs ();;

  let rec suppress c cd =
    match cd with
    | []   -> []
    | h::t -> if c = h || flip c = h then 
                t 
              else 
                h::suppress c t ;;

  let rec string_of_dominoes = function 
  | []                    -> ""
  | D(x,y)::l when l = [] -> Printf.sprintf "%d-%d" x y
  | D(x,y)::l             -> Printf.sprintf "%d-%d %s" x y (string_of_dominoes l)
  ;;
  
  (* TODO déplacer input_move à la fin pour tester sur VS les fonctions (il ne connait pas string_of_dominoes par ex qui est définie plus loin) *)
 let input_move select_domino select_end chain lst = 
    match possible_dominoes lst chain with
    | [] -> None
    | dominoes_list ->  
      let selected_domino = select_domino dominoes_list in
        let possible_chains = legal_adds selected_domino chain in 
          match List.length (possible_chains) with
          | 1 -> let () = print_endline ("Coup forcé : " ^ string_of_dominoes [selected_domino]) in 
            Some (suppress selected_domino lst, List.nth possible_chains 0)
          | _ ->  let selected_chain =  
                    let get_chain = (List.nth (possible_chains)) 
                    in select_end (get_chain 0) (get_chain 1) 
                  in Some (suppress selected_domino lst, selected_chain);;
                          
  let input_bot_move chain list =
    match possible_dominoes list chain with
    | [] -> None
    | dominoes_list ->  
        let d_index = Random.int (List.length dominoes_list) in
          let possible_chains = legal_adds (List.nth dominoes_list d_index) chain in
            match Random.int (List.length possible_chains) with
            | 0 -> input_move (function l -> List.nth l d_index) (fun _ dc2 -> dc2) chain list
            | _ -> input_move (function l -> List.nth l d_index) (fun dc1 _ -> dc1) chain list 
  ;;

  let domino_of_string saisie = 
    D(int_of_char saisie.[0] - 48, int_of_char saisie.[1] - 48)
  ;;
  
  let rec find x lst =
    match lst with
    | [] -> -1
    | h :: t -> 
        if x = h 
          then 0 
        else
          1 + find x t
  ;;
  
  (*TODO identer la fonction please*)
  let input_human_move chain list = 
    let list_dominoes = possible_dominoes list chain in
      match List.length list_dominoes with
      | 0 ->  input_move 
                (function l -> failwith "Should not occur")
                (fun dc1 dc2 -> failwith "Should not occur")
                chain
                list
      | _ ->  input_valid 
                "Sélectionner votre domino :" 
                (function str -> let domino = domino_of_string str in (List.mem domino list_dominoes) = true)
                (function str -> let domino = domino_of_string str in 
                  let possible_chains = (legal_adds (domino) chain) in 
                  (input_valid
                    "A quel bout ?"
                    (function char ->  match (domino) with
                      | d when (List.mem (append (d, chain, char.[0])) possible_chains) = true -> let chain_of_d = append (d, chain, char.[0])              in (List.mem chain_of_d possible_chains) = true
                      | toReverse                                                              -> let chain_of_d = append (flip toReverse, chain, char.[0]) in (List.mem chain_of_d possible_chains) = true)
                    (function 
                      | "<" ->  input_move 
                                  (function l -> List.nth l (find (domino) list_dominoes))
                                  (fun dc1 _ -> dc1)
                                  chain
                                  list
                      | ">" ->  input_move 
                                  (function l -> List.nth l (find (domino) list_dominoes))
                                  (fun _ dc2 -> dc2)
                                  chain
                                  list
                      | _   ->  input_move 
                                  (function l -> failwith "Should not occur")
                                  (fun dc1 dc2 -> failwith "Should not occur")
                                  chain
                                  list
                    )
                  )
                )
  ;;


(* SECTION 3 : Gestion complète d'un coup, avec affichages et pioche éventuelle *)
  
  let string_of_player = function 
    | H n when (1 <= n && n <= 4) -> Printf.sprintf"Joueur %d (humain)" n
    | B n when (1 <= n && n <= 4) -> Printf.sprintf"Joueur %d (bot)   " n 
    | _                           -> failwith "Le joueur doit etre un nombre en 1 et 4"
  ;;

  let rec take l1 n l2 = 
    match (l1, n, l2) with 
    |(l1, n, []) -> (l1, [])
    |(l1, 0, l2) -> (l1, l2)
    |(l1, n, (D(x,y))::l) -> take ((D(x,y))::l1) (n-1) l
  ;;
  
  (* move *)

  

(* SECTION 4 : Mise en place d'une partie *)

  let char_list_of_string str =
    let rec urs n l =
      if n < 0 
        then l 
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
      | 0 when x=0 -> l @ [D(0,0)]
      | 0          -> urs (l @ [D(x,0)]) (x-1) (x-1)
      | y          -> urs (l @ [D(x,y)])  x    (y-1)
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
      let player_count = List.length players_list in
        let hand_size = get_hand_size player_count in
          let rec urs l1 hand_size l2 iter_player result =
            match (take l1 hand_size l2) with
            | (l, stack) when iter_player = player_count -> (l @ stack, result)
            | (l, stack) -> urs [] hand_size stack (iter_player+1) (result @ [(l, List.nth players_list iter_player)] )
          in urs [] hand_size dominoes_list 0 []
  ;;

(* SECTION 5 : Jeu proprement dit *)
  let string_of_chain = function
    | S (_, str, _) -> str
    | _             -> ""
  ;;

  let string_of_player = function 
    | H n when (n >= 1 && n <= 4)-> Printf.sprintf "Joueur %d (humain)" n
    | B n when (n >= 1 && n <= 4)-> Printf.sprintf "Joueur %d (bot)   " n 
    | _ -> failwith "Le joueur doit etre un nombre en 1 et 4"
  ;;

  let string_of_state (dominoes_list, player)= Printf.sprintf "%s:\t%s" (string_of_player player) (string_of_dominoes dominoes_list)
  ;;
  
  let list_shuffle ls =
    let x = List.map (function y -> (Random.bits (), y)) ls in
      let comparList = List.sort compare x in
        List.map snd comparList
  ;;       

  (* play *)