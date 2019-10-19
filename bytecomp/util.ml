open Virtual
open VM

open Printf

let rec print_insts insts =
  match insts with
  | [] -> ()
  | hd :: tl ->
     match hd with
     | Literal i ->
        print_int i; print_newline ();
        print_insts tl
     | Lref s | Ldef s ->
        failwith "Lref or Ldef is still remained"
     | _ ->
        (if List.assoc hd has_args then
           (print_int (int_of_inst hd); print_string " ")
         else
           print_int (int_of_inst hd); print_newline ());
        print_insts tl


let print_code insts =
  let rec loop i = function
    | [] -> ()
    | Literal j :: tl ->
       (printf "code.(%d) <- %d;\n" i j;
        loop (i+1) tl)
    | Lref _ :: tl | Ldef _ :: tl ->
       failwith "Lref or Ldef is still remained"
    | hd :: tl ->
       if List.assoc hd has_args then
         (printf "code.(%d) <- %d; " i (int_of_inst hd))
       else
         (printf "code.(%d) <- %d;\n" i (int_of_inst hd));
       loop (i+1) tl
  in
  printf "let code = Array.make %d %d in\n" (List.length insts) 0;
  loop 0 insts; flush stdout
