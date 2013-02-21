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

let is_double_quote c =
  c = "\"".[0] ;;

let is_delimiter c =
  isspace c || c = '(' || c = ')' || is_double_quote c || c = ';' ;;

type lisp_object =
  | Fixnum of int
  | Character of char
  | String of string
  | EmptyList
  | Pair of lisp_object * lisp_object
  | Boolean of bool ;;

let eat_expected_string in_channel str =
  let aux c =
    if c != (getc in_channel)
    then (Printf.fprintf stderr "unexpcted character '%c'\n" c; raise Exit)
  in String.iter aux str ;;

let peek_expected_delimiter in_channel =
  let c = peek in_channel
  in if not (is_delimiter c) then failwith "character not followed by delimiter\n" ;;

let read_character in_channel =
  try
    match (getc in_channel) with
    | 's' -> if 'p' = (peek in_channel)
    then begin
      eat_expected_string in_channel "pace";
      peek_expected_delimiter in_channel;
      Character ' '
    end
    else begin
      peek_expected_delimiter in_channel;
      Character 's'
    end
    | 'n' -> if 'e' = (peek in_channel)
    then begin
      eat_expected_string in_channel "ewline";
      peek_expected_delimiter in_channel;
      Character '\n'
    end
    else begin
      peek_expected_delimiter in_channel;
      Character 'n'
    end
    | c -> Character c
  with End_of_file ->
    (prerr_string "incomplete character literal\n"; raise Exit) ;;

let read_string in_channel =
  let buf = Buffer.create 80
  and c = ref (getc in_channel)
  in begin
    while not (is_double_quote !c) do
      if !c = '\\' then (c := getc in_channel; if !c = 'n' then c := '\n');
      Buffer.add_char buf !c;
      c := getc in_channel
    done;
    String (Buffer.contents buf)
  end ;;

let read_fixnum in_channel c =
  let sign = if c = '-' then -1 else (ungetc c in_channel; 1)
  and num = ref 0
  in begin
    let c = ref (getc in_channel)
    in begin
      while isdigit !c do
        num := !num * 10 + (Char.code !c) - (Char.code '0');
        c := getc in_channel
      done;
      num := !num * sign;
      if is_delimiter !c
      then (ungetc !c in_channel; Fixnum !num)
      else (prerr_string "number not followed by delimiter\n"; raise Exit)
    end
  end ;;

(* mutually recursive *)
let rec read_pair in_channel =
  begin
    eat_whitespace in_channel;
    let c = ref (getc in_channel)
    in if !c = ')'
    then EmptyList
    else begin
      ungetc !c in_channel;
      let car = read in_channel
      in begin
        eat_whitespace in_channel;
        c := getc in_channel;
        if !c = '.'
        then begin
          c := peek in_channel;
          if not (isspace !c)
          then (prerr_string "dot not followed by whitespace\n"; raise Exit);
          let cdr = read in_channel
          in begin
            eat_whitespace in_channel;
            c := getc in_channel;
            if !c != ')'
            then (prerr_string "where was the trailing right paren?\n"; raise Exit);
            Pair (car, cdr)
          end
        end
        else begin
          ungetc !c in_channel;
          Pair (car, read_pair in_channel)
        end
      end
    end
  end

and read in_channel =
  try
    eat_whitespace in_channel;
    match (getc in_channel) with
    | '#' -> begin
        match (getc in_channel) with
        | 't' -> Boolean true
        | 'f' -> Boolean false
        | '\\' -> read_character in_channel
        | _ -> (prerr_string "Unknown boolean literal\n"; raise Exit)
    end
    | c when isdigit c || (c = '-' && isdigit (peek in_channel)) ->
        read_fixnum in_channel c
    | c when c = "\"".[0] -> read_string in_channel
    | '(' -> read_pair in_channel
    | ch -> (Printf.fprintf stderr "bad input. Unexpected '%c'\n" ch; raise Exit)
  with End_of_file -> (prerr_string "read illegal state\n"; raise Exit) ;;

let eval exp = exp ;;

let is_false obj =
  obj = Boolean false ;;

let write_string str =
  let dq = "\"".[0]
  in begin
    print_char dq;
    String.iter
      (fun c ->
        match c with
          '\n' -> print_string "\\n"
        | '\\' -> print_string "\\\\"
        | c when c = dq -> print_string "\\\""
        | c -> print_char c)
      str;
    print_char dq
  end ;;

(* mutually recursive *)
let rec write_pair = function
    Pair(car, cdr) -> begin
      write car;
      match cdr with
        EmptyList -> ()
      | Pair(_, _) -> (print_char ' '; write_pair cdr)
      | _ -> (print_string " . "; write cdr)
    end
  | _ -> (prerr_string "not a pair"; raise Exit)

and write obj =
  match obj with
  | Fixnum num -> Printf.printf "%d" num
  | Boolean true -> print_string "#t"
  | Boolean false -> print_string "#f"
  | Character '\n' -> print_string "#\\newline"
  | Character ' ' -> print_string "#\\space"
  | Character c -> Printf.printf "#\\%c" c
  | EmptyList -> print_string "()"
  | Pair _ -> (print_char '('; write_pair obj; print_char ')')
  | String str -> write_string str ;;

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
