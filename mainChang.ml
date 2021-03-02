
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
