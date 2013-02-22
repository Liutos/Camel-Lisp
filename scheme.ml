let input_buffers = Hashtbl.create 11 ;;

(* model *)

type lisp_object =
  | Fixnum of int
  | Character of char
  | String of string
  | EmptyList
  | Pair of lisp_object * lisp_object
  | Symbol of string
  | Boolean of bool ;;

type environment =
    EmptyEnvironment
  | Environment of (lisp_object, lisp_object) Hashtbl.t * environment ;;

let symbol_table = Hashtbl.create 20 ;;

let empty_environment = EmptyEnvironment ;;

let extend_environment vars vals env =
  let rec frame = Hashtbl.create 5
  and aux vars vals =
    match vars with
      EmptyList -> ()
    | Pair(var, rest) -> begin
        Hashtbl.add frame var (car vals);
        aux rest (cdr vals)
    end
    | _ -> (prerr_string "vals must be type Pair"; raise Exit)
  in begin
    aux vars vals;
    Environment(frame, env)
  end ;;

let setup_environment () =
  extend_environment EmptyList EmptyList EmptyEnvironment ;;

let make_symbol name =
  try
    Hashtbl.find symbol_table name
  with Not_found ->
    let symbol = Symbol name
    in begin
      Hashtbl.add symbol_table name symbol;
      symbol
    end ;;

let car = function
    Pair(car, _) -> car
  | _ -> (prerr_string "argument is not a Pair"; raise Exit) ;;

let cdr = function
    Pair(_, cdr) -> cdr
  | _ -> (prerr_string "argument is not a Pair"; raise Exit) ;;

let first_frame = function
    EmptyEnvironment -> (prerr_string "Empty environment already\n"; raise Exit)
  | Environment(frame, _) -> frame ;;

let add_binding_to_frame var value frame =
  Hashtbl.add frame var value ;;

let rec lookup_variable_value var = function
    EmptyEnvironment -> failwith "Unbound variable.\n"
  | Environment(frame, env) ->
      try
        Hashtbl.find frame var
      with Not_found -> lookup_variable_value var env ;;

let rec set_variable_value var value = function
    EmptyEnvironment -> failwith "Unbound variable.\n"
  | Environment(frame, env) ->
      try
        ignore(Hashtbl.find frame var);
        Hashtbl.add frame var value
      with Not_found -> set_variable_value var value env ;;

(* read *)

let find_or_create_stack input =
  try
    Hashtbl.find input_buffers input
  with Not_found ->
    let stack = Stack.create ()
    in begin
      Hashtbl.add input_buffers input stack;
      stack
    end ;;

let getc input_stream =
  let stack = find_or_create_stack input_stream
  in if Stack.is_empty stack
  then Stream.next input_stream
  else Stack.pop stack ;;

let ungetc c input_stream =
  let stack = find_or_create_stack input_stream
  in Stack.push c stack ;;

let isspace c =
  match c with
    ' ' | '\n' | '\r' | '\t' -> true
  | _ -> false ;;

let isdigit c =
  match c with
    '0'..'9' -> true
  | _ -> false ;;

let rec eat_whitespace input_stream =
  try
    match (getc input_stream) with
      c1 when isspace c1 -> eat_whitespace input_stream
    | ';' -> let c = ref (getc input_stream)
    in begin
      while !c != '\n' do
        c := getc input_stream
      done;
      eat_whitespace input_stream
    end
    | c2 -> ungetc c2 input_stream
  with Stream.Failure -> () ;;

let peek input_stream =
  let c = getc input_stream
  in (ungetc c input_stream; c) ;;

let is_double_quote c =
  c = "\"".[0] ;;

let is_delimiter c =
  isspace c || c = '(' || c = ')' || is_double_quote c || c = ';' ;;

let eat_expected_string in_stream str =
  let aux c =
    if c != (getc in_stream)
    then (Printf.fprintf stderr "unexpcted character '%c'\n" c; raise Exit)
  in String.iter aux str ;;

let peek_expected_delimiter in_stream =
  let c = peek in_stream
  in if not (is_delimiter c)
  then failwith "character not followed by delimiter\n" ;;

let read_character in_stream =
  try
    match (getc in_stream) with
    | 's' -> if 'p' = (peek in_stream)
    then begin
      eat_expected_string in_stream "pace";
      peek_expected_delimiter in_stream;
      Character ' '
    end
    else begin
      peek_expected_delimiter in_stream;
      Character 's'
    end
    | 'n' -> if 'e' = (peek in_stream)
    then begin
      eat_expected_string in_stream "ewline";
      peek_expected_delimiter in_stream;
      Character '\n'
    end
    else begin
      peek_expected_delimiter in_stream;
      Character 'n'
    end
    | c -> Character c
  with Stream.Failure ->
    (prerr_string "incomplete character literal\n"; raise Exit) ;;

let read_string in_stream =
  let buf = Buffer.create 80
  and c = ref (getc in_stream)
  in begin
    while not (is_double_quote !c) do
      if !c = '\\' then (c := getc in_stream; if !c = 'n' then c := '\n');
      Buffer.add_char buf !c;
      c := getc in_stream
    done;
    String (Buffer.contents buf)
  end ;;

let read_fixnum in_stream c =
  let sign = if c = '-' then -1 else (ungetc c in_stream; 1)
  and num = ref 0
  in try
    let c = ref (getc in_stream)
    in begin
      while isdigit !c do
        num := !num * 10 + (Char.code !c) - (Char.code '0');
        c := getc in_stream
      done;
      num := !num * sign;
      if is_delimiter !c
      then (ungetc !c in_stream; Fixnum !num)
      else (prerr_string "number not followed by delimiter\n"; raise Exit)
    end
  with Stream.Failure -> Fixnum !num ;;

let is_initial = function
    'a'..'z' | 'A'..'Z' | '*' | '/' | '>' | '<' | '=' | '?' | '!' -> true
  | _ -> false ;;

let read_symbol in_stream init =
  let buf = Buffer.create 10
  in let c = ref init
  in begin
    while is_initial !c || isdigit !c || !c = '+' || !c = '-' do
      Buffer.add_char buf !c;
      c := getc in_stream
    done;
    if is_delimiter !c
    then (ungetc !c in_stream; make_symbol (Buffer.contents buf))
    else (Printf.fprintf stderr "symbol not followed by delimiter. Found '%c'\n" !c; raise Exit)
  end ;;

(* mutually recursive: read_pair <-> read *)
let rec read_pair in_stream =
  begin
    eat_whitespace in_stream;
    let c = ref (getc in_stream)
    in if !c = ')'
    then EmptyList
    else begin
      ungetc !c in_stream;
      let car = read in_stream
      in begin
        eat_whitespace in_stream;
        c := getc in_stream;
        if !c = '.'
        then begin
          c := peek in_stream;
          if not (isspace !c)
          then (prerr_string "dot not followed by whitespace\n"; raise Exit);
          let cdr = read in_stream
          in begin
            eat_whitespace in_stream;
            c := getc in_stream;
            if !c != ')'
            then (prerr_string "where was the trailing right paren?\n"; raise Exit);
            Pair (car, cdr)
          end
        end
        else begin
          ungetc !c in_stream;
          Pair (car, read_pair in_stream)
        end
      end
    end
  end

and read in_stream =
  try
    eat_whitespace in_stream;
    match (getc in_stream) with
    | '#' -> begin
        match (getc in_stream) with
        | 't' -> Boolean true
        | 'f' -> Boolean false
        | '\\' -> read_character in_stream
        | _ -> (prerr_string "Unknown boolean literal\n"; raise Exit)
    end
    | '(' -> read_pair in_stream
    | '\'' -> Pair(make_symbol "quote", Pair(read in_stream, EmptyList))
    | c when isdigit c || (c = '-' && isdigit (peek in_stream)) ->
        read_fixnum in_stream c
    | c when is_double_quote c -> read_string in_stream
    | c when is_initial c || ((c = '+' || c = '-') && is_delimiter (peek in_stream)) -> read_symbol in_stream c
    | c -> (Printf.fprintf stderr "bad input. Unexpected '%c'\n" c; raise Exit)
  with End_of_file -> (prerr_string "read illegal state\n"; raise Exit) ;;

(* eval *)

let is_self_evaluating = function
    Pair(_, _) | Symbol _ -> false
  | _ -> true ;;

let is_tagged_list exp tag =
  match exp with
    Pair(car, _) -> car == tag
  | _ -> false ;;

let is_quoted exp =
  is_tagged_list exp (make_symbol "quote") ;;

let text_of_quotation = function
    Pair(_, Pair(obj, _)) -> obj
  | _ -> (prerr_string "argument is not a Pair"; raise Exit) ;;

let enclosing_environment = function
    EmptyEnvironment -> (prerr_string "Empty environment already\n"; raise Exit)
  | Environment(_, env) -> env ;;

let is_symbol = function
    Symbol _ -> true
  | _ -> false ;;

let is_variable = is_symbol ;;

let define_variable var value = function
    EmptyEnvironment -> failwith "Empty environment.\n"
  | Environment(frame, _) -> add_binding_to_frame var value frame ;;

let is_assignment exp =
  is_tagged_list exp (make_symbol "set!") ;;

let assignment_variable exp = car (cdr exp) ;;
let assignment_value exp = car (cdr (cdr exp)) ;;

let is_definition exp =
  is_tagged_list exp (make_symbol "define") ;;

let definition_variable exp = car (cdr exp) ;;
let definition_value exp = car (cdr (cdr exp)) ;;

(* mutually recursive: eval_assignment <-> eval *)
let rec eval_assignment exp env =
  begin
    set_variable_value
      (assignment_variable exp)
      (eval (assignment_value exp) env)
      env;
    make_symbol "ok"
  end

and eval_definition exp env =
  begin
    define_variable
      (definition_variable exp)
      (eval (definition_value exp) env)
      env;
    make_symbol "ok"
  end

and eval exp env =
  match exp with
    exp when is_self_evaluating exp -> exp
  | exp when is_variable exp -> lookup_variable_value exp env
  | exp when is_assignment exp -> begin
      eval_assignment exp env
  end
  | exp when is_definition exp -> begin
      eval_definition exp env
  end
  | exp when is_quoted exp -> text_of_quotation exp
  | _ -> (prerr_string "cannot eval unknown expression type\n"; raise Exit) ;;

(* print *)

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

(* mutually recursive: write_pair <-> write *)
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
  | Symbol name -> print_string name
  | String str -> write_string str ;;

let global_environment = setup_environment () ;;

(* toploop *)

let main () =
  begin
    print_string "Welcome to Bootstrap Scheme. Use ctrl-c to exit.\n";
    flush stdout;
    while true do
      print_string "> ";
      flush stdout;
      write (eval (read (Stream.of_channel stdin)) global_environment);
      print_string "\n"
    done;
    0
  end ;;

let rep () =
  write (eval (read (Stream.of_channel stdin)) global_environment) ;;

let test_repl () =
  let cases =
    ["#t ";
     "-123 ";
     "#\\c ";
     "\"asdf\" ";
     "(quote ()) ";
     "(quote (0 . 1)) ";
     "(quote (0 1 2 3)) ";
     "(quote asdf) "]
  and test case = begin
    Printf.printf "%s => " case;
    flush stdout;
    write (eval (read (Stream.of_string case)) global_environment);
    print_newline ()
  end
  in List.iter test cases ;;
