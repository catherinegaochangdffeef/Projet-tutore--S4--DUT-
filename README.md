# Projet-tutore--S4--DUT-
https://gist.github.com/laowantong/452e169d30efcda62ca069677ad62d08
## Jeu de dominos en Ocaml

Organisation des fonctions :

### 1. Gestion des coups légaux
- **flip** : domino -> domino = \<fun>  
    *permute un domino donné*  

- **append** : domino * chain * char -> chain = \<fun>  
*ajoute un domino donné à une extrémité donnée d'une chaîne donnée, sans effectuer aucun test de compatibilité sur la chaîne, le domino ou sa présentation.*  

- **legal_adds** : domino -> chain -> chain list = \<fun>  
*renvoie les chaînes de dominos résultant de toutes les poses légales et distinctes d'un domino d donné au bout d'une chaîne de dominos cd donnée.*  

- **possible_dominoes** : domino list -> chain -> domino list = \<fun>  
*renvoie la liste de chacun des dominos d'une main donnée qui est plaçable au bout d'une chaîne donnée.*

### 2. Sélection du coup à jouer et calcul du résultat
- **input_valid**  
*Cette fonction complètement générique prend en entrée un message d'invite, une fonction testant la validité de l'entrée (selon un critère quelconque) et une autre appliquant une conversion quelconque à une entrée valide avant de la renvoyer.*  

- **suppress** : domino -> domino list -> domino list = \<fun>  
*est utilisée pour rendre une copie mise à jour d'une main donnée lorsqu'on en retire un domino donné.*  

- **input_move** :
  (domino list -> domino) ->
  (chain -> chain -> chain) ->
  chain -> domino list -> (domino list * chain) option = \<fun>  
  *est une abstraction de l'acquisition d'un coup, prévue pour être utilisée aussi bien pour un joueur humain que pour un bot. La spécialisation se fait grâce aux deux premiers arguments.*    

  *Le premier argument est une fonction que j'appelle select_domino et qui, étant donné la main d'un joueur, renvoie le domino possible sélectionné par celui-ci (soit par saisie, soit par tirage). Elle ne sera pas appelée par move lorsqu'aucun domino de la main n'est possible, ou qu'un seul domino est possible.*  

  *Le deuxième argument est une fonction que j'appelle select_end et qui, étant donné deux chaînes de domino (résultant en fait de l'ajout du domino sélectionné à l'une ou l'autre extrémité de la chaîne en cours), en renvoie une (soit par saisie d'une direction '>' ou '<', soit par tirage aléatoire).*  

  *Le troisième argument est la chaîne en cours.*   

  *Le quatrième argument est la main du joueur.*  

  *Le résultat est un couple formé de la nouvelle main et de la nouvelle chaîne si un coup est possible, ou rien sinon. Le talon n'intervenant pas à ce niveau, dans ce dernier cas la pioche sera déléguée à la fonction appelante.*  


- **input_bot_move** : chain -> domino list -> (domino list * chain) option = \<fun>  
*est la spécialisation « bot » de `input_move`, avec la même sémantique pour les deux derniers arguments et le résultat.*  

- **input_human_move** : chain -> domino list -> (domino list * chain) option = \<fun>  
*est la spécialisation « human » de `input_move`, avec la même sémantique pour les deux derniers arguments et le résultat.*  


### 3. Gestion complète d'un coup, avec affichages et pioche éventuelle
- **string_of_player** : player -> string = \<fun>  
*calcule simplement la chaîne de caractères à afficher pour désigner un joueur.*  

- **string_of_dominoes** : domino list -> string = \<fun>  
*calcule simplement la chaîne de caractères représentant une main de dominos.*  

- **take** : domino list -> int -> domino list -> domino list * domino list = \<fun>  
*transfère un nombre donné de dominos d'une liste donnée (talon) à une autre (main).*  

- **move** :
  domino list ->
  chain ->
  domino list ->
  player ->
  domino list * chain * domino list = \<fun>  
  *prend un talon, une chaîne, une main et un joueur, et renvoie le talon, la chaîne et la main résultant d'un nouveau coup, non sans produire en effet de bord tous les affichages associés.*  


### 4. Mise en place d'une partie
- **make_dominoes** : int -> domino list = \<fun>  
*génère sous forme de liste l'ensemble des dominos de plus haut chiffre donné.*  

- **char_list_of_string** : string -> char list = \<fun>  
*renvoie la liste des caractères d'une chaîne de caractères donnée.*  

- **players_of_string** : string -> player list = \<fun>  
*convertit une chaîne formée de caractères ``B`` et ``H`` en une chaîne de valeurs de type ``player`` numérotées séquentiellement.*  

- **get_hand_size** : int -> int = \<fun>  
*envoie le nombre de dominos dans la main initiale d'un joueur en fonction du nombre de ceux-ci. Elle doit échouer avec le message ``"Entre 2 et 4 joueurs, please!"`` pour tout autre entier que 2, 3 et 4.*  

- **make_state_list** : string -> domino list -> domino list * (domino list * player) list = \<fun>  
*prend une chaîne formée de caractères B et H, et la liste des dominos du jeu (résultat de ``make_dominoes``), et construit le talon initial, ainsi que la liste des états initiaux, un état étant le couple formé par un joueur et sa main.*  


### 5. Jeu proprement dit
- **string_of_chain** : chain -> string = \<fun>  
*extrait simplement le deuxième membre du triplet d'une chaîne de dominos donnée, ou à défaut renvoie la chaîne de caractères vide.*  

- **string_of_state** : domino list * player -> string = \<fun>  
*calcule la chaîne de caractères représentant l'état d'un joueur.* 

- **list_shuffle** : 'a list -> 'a list = \<fun>  
*renvoie une copie mélangée d'une liste donnée.*  

- **play** : int -> string -> unit = \<fun>  
*contrôle le déroulement d'une partie à partir d'une chaîne de caractères `'B'` et `'H'` et du plus haut chiffre d'un domino. Elle mélange ces dominos, les distribue aux joueurs, affiche la configuration initiale, déroule le jeu et affiche à la fin quel joueur a gagné ou, le cas échéant, ``"Match nul!"``.*
