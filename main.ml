let read = ref read_line;
let rec input_valid prompt is_valid cast =
  let rec urs () =
    let s = !read () in
    if is_valid s then cast s
    else
      let () = print_endline "Réessayez!" in
      urs ()
  in
  print_endline prompt; urs ();