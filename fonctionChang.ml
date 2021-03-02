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