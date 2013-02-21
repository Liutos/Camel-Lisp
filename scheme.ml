let input_buffers = Hashtbl.create 11 ;;

let find_or_create_stack input =
  try
    Hashtbl.find input_buffers input
  with Not_found ->
    let stack = Stack.create ()
    in begin
      Hashtbl.add input_buffers input stack;
      stack
    end ;;

let getc input_channel =
  let stack = find_or_create_stack input_channel
  in if Stack.is_empty stack
  then input_char input_channel
  else Stack.pop stack ;;

let ungetc c input_channel =
  let stack = find_or_create_stack input_channel
  in Stack.push c stack ;;

let isspace c =
  match c with
    ' ' | '\n' | '\r' | '\t' -> true
  | _ -> false ;;

let isdigit c =
  match c with
    '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
  | _ -> false ;;

let rec eat_whitespace input_channel =
  try
    match (getc input_channel) with
      c1 when isspace c1 -> eat_whitespace input_channel
    | ';' -> let c = ref (getc input_channel)
    in begin
      while !c != '\n' do
        c := getc input_channel
      done;
      eat_whitespace input_channel
    end
    | c2 -> ungetc c2 input_channel
  with End_of_file -> () ;;

let peek input_channel =
  let c = getc input_channel
  in (ungetc c input_channel; c) ;;

let is_delimiter c =
  isspace c || c = '(' || c = ')' || c = "\"".[0] || c = ';' ;;

type lisp_object =
    Fixnum of int
  | Boolean of bool ;;

let rec read in_channel =
  try
    (* eat_whitespace in_channel; *)
    match (getc in_channel) with
    | c1 when isspace c1 -> read in_channel
    | '#' -> match (getc in_channel) with
      | 't' -> Boolean true
      | 'f' -> Boolean false
      | _ -> (prerr_string "Unknown boolean literal\n"; exit 1)
    | c when isdigit c || (c = '-' && isdigit (peek in_channel)) ->
        let sign = ref 1
        and num = ref 0
        in begin
          if c = '-' then sign := -1 else ungetc c in_channel;
          let c = ref (getc in_channel)
          in begin
            while isdigit !c do
              num := !num * 10 + (Char.code !c) - (Char.code '0');
              c := getc in_channel
            done;
            num := !num * !sign;
            if is_delimiter !c
            then (ungetc !c in_channel; Fixnum !num)
            else begin
              prerr_string "number not followed by delimiter\n";
              exit 1
            end
          end
        end
    | ch -> (Printf.fprintf stderr "bad input. Unexpected '%c'\n" ch; exit 1)
  with End_of_file -> (prerr_string "read illegal state\n"; exit 1) ;;

let eval exp = exp ;;

let is_false obj =
  obj = Boolean false ;;

let write obj =
  match obj with
  | Fixnum num -> Printf.printf "%d" num
  | Boolean _ -> Printf.printf "#%c" (if is_false obj then 'f' else 't')
  | _ -> (prerr_string "cannot write unknown type\n"; exit 1) ;;

let main () =
  begin
    print_string "Welcome to Bootstrap Scheme. Use ctrl-c to exit.\n";
    while true do
      print_string "> ";
      write (eval (read stdin));
      print_string "\n"
    done;
    0
  end ;;
