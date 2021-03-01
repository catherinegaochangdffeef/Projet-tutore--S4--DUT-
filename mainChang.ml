
type domino = D of int * int



type chain = E | S of int * string * int



type player = H of int | B of int


let flip (D(x,y)) = D(y,x)



let append (D (d1,d2), chain, char) = 
  let concaten = ((string_of_int d1)^"-"^ (string_of_int d2)) in
    match (D (d1,d2), chain, char) with
    | (D (d1,d2), E, _) -> S (d1, concaten, d2)
    | (D (d1,_), S (_, str, fin), '<') -> S (d1, concaten ^ " " ^ str , fin)
    | (D (_,d2), S (debut, str, _), '>') -> S (debut, str ^ " " ^ concaten , d2)
    | (_,_,_) -> E

let legal_adds (D(d1,d2), chain ) =
  let concaten = ((string_of_int d1)^"-"^ (string_of_int d2)) in
  let concaten2=((string_of_int d2)^"-"^ (string_of_int d1)) in
   match (D(d1,d2), chain ) with
   |(D(d1,d2),E) -> [S(d1,concaten,d2)]
   |(D(d1,d2), S(d1,str,fin)) ->[S(d2,concaten2 ^ " " ^ str , fin )]
   |(D(d1,d2), S(d2,str,fin)) ->[S(d1,concaten ^ " " ^ str , fin )]
   |(D(d1,d2), S(d1,str,d2))->[S(d2,concaten2 ^ " " ^ str , d2 );
   S(d1, str^ " " ^ concaten2 , d1 )]
   |(D(_,_),_) ->[]
