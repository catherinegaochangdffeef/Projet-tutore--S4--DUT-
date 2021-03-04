(*open Main if we use the ocaml compiler we have to use this with the compile code : "ocamlc -o nameofexe main.ml test.ml" *)
#use "main.ml"  (* #use can be use only in toplevel, not when we compil *)

let nb_tests_ok = ref 0;;
let nb_tests = ref 0;;
let test label expected calculated =
    incr nb_tests;
    try
        assert (Lazy.force expected = calculated);
        print_endline ("OK ........ " ^ label);
        incr nb_tests_ok
    with
        | Assert_failure _ -> print_endline ("FAILURE ... " ^ label)
        | _ -> print_endline ("ERROR ..... " ^ label)
;;
test
    "append: ajout d'un domino à droite d'une chaîne non vide"
    (lazy (append (D (3, 5), S (1, "1-2 4-3", 3), '>')))
    (S (1, "1-2 4-3 3-5", 5))
; test
    "append: ajout d'un domino à gauche d'une chaîne non vide"
    (lazy (append (D (5, 1), S (1, "1-2 4-3", 3), '<')))
    (S (5, "5-1 1-2 4-3", 3))
; test
    "append: aucun contrôle n'est fait sur la compatibilité du domino poussé"
    (lazy (append (D (5, 2), S (1, "1-2 4-3", 3), '<')))
    (S (5, "5-2 1-2 4-3", 3))
; test
    "char_list_of_string: conversion d'une chaîne non vide en liste de caractères"
    (lazy (char_list_of_string "foobar"))
    ['f';'o';'o';'b';'a';'r']
 ; test
    "char_list_of_string: conversion d'une vide en liste vide"
    (lazy (char_list_of_string ""))
    []
; test
    "flip: changement de présentation d'un domino non symétrique"
    (lazy (flip (D (2,3))))
    (D (3,2))
; test
    "flip: changement de présentation d'un domino symétrique"
    (lazy (flip (D (3,3))))
    (D (3,3))
; test
    "get_hand_size: deux joueurs reçoivent chacun une main de sept dominos"
    (lazy (get_hand_size 2))
    7
; test
    "get_hand_size: quatre joueurs reçoivent chacun une main de six dominos"
    (lazy (get_hand_size 4))
    6
; test
    "get_hand_size: trois joueurs reçoivent chacun une main de six dominos"
    (lazy (get_hand_size 3))
    6
    (*
; Random.init 42
; test
    "input_bot_move: vérifier l'effet de bord: affichage de 6-5 comme coup forcé"
    (lazy (input_bot_move (S (6, "6-1 1-5", 5)) [D (6, 5)]))
    (Some ([], S (5, "5-6 6-1 1-5", 5)))
; test
    "input_bot_move: vérifier l'effet de bord: affichage de 6-5 comme coup forcé"
    (lazy (input_bot_move (S (6, "6-1 1-4", 4)) [D (6, 5)]))
    (Some ([], S (5, "5-6 6-1 1-4", 4)))
; test
    "input_bot_move: vérifier l'effet de bord: affichage de 4-2 comme coup à placer"
    (lazy (input_bot_move (S (6, "6-1 1-4", 4)) [D (6, 3); D (4, 2)]))
    (Some ([D (6, 3)], S (6, "6-1 1-4 4-2", 2)))
; test
    "input_bot_move: pas de coup possible"
    (lazy (input_bot_move (S (6, "6-1 1-5", 5)) [D (4, 4)]))
    None
; read := (let x = Stream.of_list ["356"; "12"; "64"; "foobar"; ">"] in function () -> Stream.next x)
; test
    "input_human_move: simulation des saisies du joueur, sélection du second domino possible, et placement à droite"
    (lazy (input_human_move
            (S (6, "6-1 1-4", 4))
            [D (1, 2); D (6, 5); D (6, 4)]))
    (Some ([D (1, 2); D (6, 5)], S (6, "6-1 1-4 4-6", 6))) *)
; Random.init 42
; test
    "input_move: pas de domino possible, les fonctions de sélection sont inutilisées"
    (lazy (input_move
            (function l -> failwith "should not occur")
            (fun dc1 dc2 -> failwith "should not occur")
            (S (6, "6-1 1-4", 4))
            [D (1, 2); D (3, 5); D (5, 3)]))
    None
; test
    "input_move: sélection du seul domino possible, vérifier l'effet de bord: affichage de 6-5 comme coup forcé"
    (lazy (input_move
            (function l -> List.nth l 0)
            (fun dc1 dc2 -> failwith "should not occur")
            (S (6, "6-1 1-4", 4))
            [D (1, 2); D (6, 5); D (5, 3)]))
    (Some ([D (1, 2); D (5, 3)], S (5, "5-6 6-1 1-4", 4)))
; test
    "input_move: sélection du seul domino possible, forçage de 6-4, et placement à droite"
    (lazy (input_move
            (function l -> List.nth l 0)
            (fun _ dc2 -> dc2)
            (S (6, "6-1 1-4", 4))
            [D (1, 2); D (6, 4); D (5, 3)]))
    (Some ([D (1, 2); D (5, 3)], S (6, "6-1 1-4 4-6", 6)))
; test
    "input_move: sélection du seul domino possible, forçage de 6-4, et placement à droite"
    (lazy (input_move
            (function l -> List.nth l 0)
            (fun dc1 _ -> dc1)
            (S (6, "6-1 1-4", 4))
            [D (1, 2); D (6, 4); D (5, 3)]))
    (Some ([D (1, 2); D (5, 3)], S (4, "4-6 6-1 1-4", 4)))
; test
    "input_move: sélection du premier domino possible"
    (lazy (input_move
            (function l -> List.nth l 0)
            (fun dc1 dc2 -> failwith "should not occur")
            (S (6, "6-1 1-4", 4))
            [D (1, 2); D (6, 5); D (4, 3)]))
    (Some ([D (1, 2); D (4, 3)], S (5, "5-6 6-1 1-4", 4)))
; test
    "input_move: sélection du second domino possible"
    (lazy (input_move
            (function l -> List.nth l 1)
            (fun dc1 dc2 -> failwith "should not occur")
            (S (6, "6-1 1-4", 4))
            [D (1, 2); D (6, 5); D (4, 3)]))
    (Some ([D (1, 2); D (6, 5)], S (6, "6-1 1-4 4-3", 3)))
; test
    "input_move: sélection du second domino possible, et placement à gauche"
    (lazy (input_move
            (function l -> List.nth l 1)
            (fun dc1 _ -> dc1)
            (S (6, "6-1 1-4", 4))
            [D (1, 2); D (6, 5); D (6, 4)]))
    (Some ([D (1, 2); D (6, 5)], S (4, "4-6 6-1 1-4", 4)))
; test
    "input_move: sélection du second domino possible, et placement à droite"
    (lazy (input_move
            (function l -> List.nth l 1)
            (fun _ dc2 -> dc2)
            (S (6, "6-1 1-4", 4))
            [D (1, 2); D (6, 5); D (6, 4)]))
    (Some ([D (1, 2); D (6, 5)], S (6, "6-1 1-4 4-6", 6)))

; read := (let x = Stream.of_list ["-10"; "500"; "10"] in function () -> Stream.next x)
; test
    "input_valid: simulation de la saisie jusqu'à validité d'un âge réaliste"
    (lazy (
        input_valid
            "Votre âge?"
            (function x -> let n = int_of_string x in n >= 0 && n < 120)
            (function x -> 1.2 *. float_of_string x |> int_of_float)))
    12
; test
    "legal_adds: pose d'un domino initial"
    (lazy (legal_adds (D (5, 6)) E))
    [S (5, "5-6", 6)]
; test
    "legal_adds: pose d'un double à droite"
    (lazy (legal_adds (D (6, 6)) (S (1, "1-0 0-6", 6))))
    [ S (1, "1-0 0-6 6-6", 6) ]
; test
    "legal_adds: pose d'un double à gauche"
    (lazy (legal_adds (D (6, 6)) (S (6, "6-0 0-1", 1))))
    [ S (6, "6-6 6-0 0-1", 1) ]
; test
    "legal_adds: pose d'un double possible aux deux bouts, une seule possibilité retenue"
    (lazy (legal_adds (D (6, 6)) (S (6, "6-1 1-6", 6))))
    [S (6, "6-6 6-1 1-6", 6)]
; test
    "legal_adds: pose d'un non-double à droite, présentation permutée"
    (lazy (legal_adds (D (4, 2)) (S (5, "5-5 5-3 3-2", 2))))
    [S (5, "5-5 5-3 3-2 2-4", 4)]
; test
    "legal_adds: pose d'un non-double à droite"
    (lazy (legal_adds (D (2, 4)) (S (5, "5-5 5-3 3-2", 2))))
    [S (5, "5-5 5-3 3-2 2-4", 4)]
; test
    "legal_adds: pose d'un non-double à gauche, présentation permutée"
    (lazy (legal_adds (D (5, 1)) (S (5, "5-5 5-3 3-2", 2))))
    [S (1, "1-5 5-5 5-3 3-2", 2)]
; test
    "legal_adds: pose d'un non-double à gauche"
    (lazy (legal_adds (D (1, 5)) (S (5, "5-5 5-3 3-2", 2))))
    [S (1, "1-5 5-5 5-3 3-2", 2)]
; test
    "legal_adds: pose d'un non-double possible aux deux bouts, présentation permutée"
    (lazy (legal_adds (D (6, 5)) (S (6, "6-1 1-5", 5))))
    [ S (5, "5-6 6-1 1-5", 5); S (6, "6-1 1-5 5-6", 6) ]
; test
    "legal_adds: pose d'un non-double possible aux deux bouts, une seule possibilité retenue"
    (lazy (legal_adds (D (5, 6)) (S (6, "6-1 1-6", 6))))
    [S (5, "5-6 6-1 1-6", 6)]
; test
    "legal_adds: pose d'un non-double possible aux deux bouts"
    (lazy (legal_adds (D (5, 6)) (S (6, "6-1 1-5", 5))))
    [S (5, "5-6 6-1 1-5", 5); S (6, "6-1 1-5 5-6", 6)]
; test
    "legal_adds: pose impossible"
    (lazy (legal_adds (D (5, 6)) (S (1, "1-5 6-2", 2))))
    []
; Random.init 42
; test
    "list_shuffle"
    (lazy (list_shuffle [1; 2; 3; 4; 5; 6; 7; 8; 9]))
    [3; 5; 4; 9; 8; 6; 2; 7; 1]
; test
    "make_dominoes: de (0,0) à (3,3)"
    (lazy (make_dominoes 3))
    [D (3, 3); D (3, 2); D (3, 1); D (3, 0); D (2, 2); D (2, 1); D (2, 0); D (1, 1); D (1, 0); D (0, 0)]

; Random.init 42
; test
    "make_state_list: création d'un jeu à deux joueurs à partir d'une liste de dominos non mélangée"
    (lazy (make_state_list "HB" [D (4, 4); D (4, 3); D (4, 2); D (4, 1); D (4, 0); D (3, 3); D (3, 2); D (3, 1); D (3, 0); D (2, 2); D (2, 1); D (2, 0); D (1, 1); D (1, 0); D (0, 0)]))
    (
            [D (0, 0)], (* le talon restant après distribution *)
        [
                ( (* la main du premier joueur, et sa catégorie avec son numéro *)
                [D (3, 2); D (3, 3); D (4, 0); D (4, 1); D (4, 2); D (4, 3); D (4, 4)],
                H 1
            );
            ( (* la main du second joueur, et sa catégorie avec son numéro *)
                [D (1, 0); D (1, 1); D (2, 0); D (2, 1); D (2, 2); D (3, 0); D (3, 1)],
                B 2
            )
        ]
    )
    (*
; Random.init 42
; test
    "move: vérifier l'effet de bord: affichage de la main et de 6-5 comme coup forcé"
    (lazy (move [D (1, 1); D (1, 2); D (1, 3)] (S (6, "6-1 1-5", 5)) [D (6, 5)] (B 1)))
    ([D (1, 1); D (1, 2); D (1, 3)], S (5, "5-6 6-1 1-5", 5), [])
; test
    "move: vérifier l'effet de bord: affichage de la main et de 6-5 comme coup forcé"
    (lazy (move [D (1, 1); D (1, 2); D (1, 3)] (S (6, "6-1 1-4", 4)) [D (6, 5)] (B 2)))
    ([D (1, 1); D (1, 2); D (1, 3)], S (5, "5-6 6-1 1-4", 4), [])
; test
    "move: vérifier l'effet de bord: affichage de la main et de 4-2 comme coup à placer"
    (lazy (move [D (1, 1); D (1, 2); D (1, 3)] (S (6, "6-1 1-4", 4)) [D (6, 3); D (4, 2)] (B 3)))
    ([D (1, 1); D (1, 2); D (1, 3)], S (6, "6-1 1-4 4-2", 2), [D (6, 3)])
; test
    "move: pas de coup possible, prise de deux dominos dans la pioche"
    (lazy (move [D (1, 1); D (1, 2); D (1, 3)] (S (6, "6-1 1-5", 5)) [D (4, 4)] (B 4)))
    ([D (1, 3)], S (6, "6-1 1-5", 5), [D (1, 2); D (1, 1); D (4, 4)])
*)
; test
    "players_of_string: construction d'une liste de 3 joueurs numérotés"
    (lazy (players_of_string "HBB"))
    [H 1; B 2; B 3]
; test
    "possible_dominoes: aucun domino d'une main n'est possible à placer"
    (lazy (possible_dominoes [D (3,3); D (5,4); D (3,3); D (5,3)] (S (2, "2-4 4-3 3-1 0-1", 6)) ))
    []
; test
    "possible_dominoes: plusieurs dominos d'une main sont possibles à plusieurs extrémités"
    (lazy (possible_dominoes [D (3,2); D (5,4); D (3,3); D (6,3)] (S (2, "2-4 4-3 3-1 0-1", 6)) ))
    [D (3,2); D (6, 3)]
; test
    "possible_dominoes: plusieurs dominos d'une main sont possibles à une extrémité"
    (lazy (possible_dominoes [D (3,2); D (5,4); D (3,3); D (6,3)] (S (3, "3-4 4-2 2-1 0-1", 1)) ))
    [D (3,2); D (3,3); D (6, 3)]
; test
    "string_of_chain: chaîne de dominos vide"
    (lazy (string_of_chain E))
    ""
; test
    "string_of_chain: chaîne de dominos non vide"
    (lazy (string_of_chain (S (4, "4-5 5-6 6-6 6-2", 2))))
    "4-5 5-6 6-6 6-2"
; test
    "string_of_state"
    (lazy (string_of_state ([D (1, 2); D (6, 5); D (4, 3)], H 1)))
    "Joueur 1 (humain):\t1-2 6-5 4-3"
; test
    "string_of_dominoes: noter qu'il y a un domino de plus que d'espaces séparateurs"
    (lazy (string_of_dominoes [D (5, 6); D (4, 4); D (3, 3); D (1, 1); D (2, 2)]))
    "5-6 4-4 3-3 1-1 2-2"
; test
    "string_of_dominoes: un domino"
    (lazy (string_of_dominoes [D (5, 6)]))
    "5-6"
; test
    "string_of_dominoes: zéro domino"
    (lazy (string_of_dominoes []))
    ""
; test
    "string_of_player: chaîne affichée pour le joueur (bot) 2"
    (lazy (string_of_player (B 2)))
    "Joueur 2 (bot)   "
; test
    "string_of_player: chaîne affichée pour le joueur (humain) 2"
    (lazy (string_of_player (H 2)))
    "Joueur 2 (humain)"
; test
    "suppress: essai de suppression d'un domino d'une liste dont il est absent"
    (lazy (suppress (D (2,3)) [D (1, 2); D (6, 5)]))
    [D (1, 2); D (6, 5)]
; test
    "suppress: essai de suppression d'un domino d'une liste vide"
    (lazy (suppress (D (2,3)) []))
    []
; test
    "suppress: suppression d'un domino à droite d'une liste, présentation permutée"
    (lazy (suppress (D (4,5)) [D (3,2); D (5,4)]))
    [D (3,2)]
; test
    "suppress: suppression d'un domino à droite d'une liste"
    (lazy (suppress (D (4,5)) [D (3,2); D (4,5)]))
    [D (3,2)]
; test
    "suppress: suppression d'un domino à gauche d'une liste, présentation permutée"
    (lazy (suppress (D (4,5)) [D (5,4); D (3,2)]))
    [D (3,2)]
; test
    "suppress: suppression d'un domino à gauche d'une liste"
    (lazy (suppress (D (4,5)) [D (4,5); D (3,2)]))
    [D (3,2)]
; test
    "suppress: suppression d'un domino à l'intérieur d'une liste"
    (lazy (suppress (D (4,5)) [D (3,2); D (5,4); D (6, 3)]))
    [D (3,2); D (6, 3)]
; test
    "suppress: suppression d'un domino à l'intérieur d'une liste, présentation permutée"
    (lazy (suppress (D (5,4)) [D (3,2); D (5,4); D (6, 3)]))
    [D (3,2); D (6, 3)]
; test
    "suppress: suppression d'un domino d'une liste réduite à ce domino, présentation permutée"
    (lazy (suppress (D (4,5)) [D (5,4)]))
    []
; test
    "suppress: suppression d'un domino d'une liste réduite à ce domino"
    (lazy (suppress (D (4,5)) [D (4,5)]))
    []
; test
    "take: passage de deux dominos du talon à la main"
    (lazy (take [D (1, 1); D (2, 2)] 2 [D (3, 3); D (4, 4); D (5, 6)]))
    ([D (4, 4); D (3, 3); D (1, 1); D (2, 2)], [D (5, 6)])
; test
    "take: passage de tous les dominos du talon à la main, dépassement ignoré"
    (lazy (take [D (1, 1); D (2, 2)] 42 [D (3, 3); D (4, 4); D (5, 6)]))
    ([D (5, 6); D (4, 4); D (3, 3); D (1, 1); D (2, 2)], [])
; test
    "take: passage de tous les dominos du talon à la main"
    (lazy (take [D (1, 1); D (2, 2)] 3 [D (3, 3); D (4, 4); D (5, 6)]))
    ([D (5, 6); D (4, 4); D (3, 3); D (1, 1); D (2, 2)], [])
; test
    "take: passage de zéro dominos du talon à la main"
    (lazy (take [D (1, 1); D (2, 2)] 0 [D (3, 3); D (4, 4); D (5, 6)]))
    ([D (1, 1); D (2, 2)], [D (3, 3); D (4, 4); D (5, 6)]) 
; print_endline (Printf.sprintf "Succès: %d%% de %d tests" (100 * !nb_tests_ok / !nb_tests) !nb_tests)