(* model *)

type lisp_object =
    Fixnum of int
  | Character of char
  | String of string
  | EmptyList
  | Pair of lisp_object * lisp_object
  | Symbol of string
  | Boolean of bool ;;

let symbol_table = Hashtbl.create 20 ;;

let car = function
    Pair(car, _) -> car
  | _ -> invalid_arg "Argument is not a Pair" ;;

let cdr = function
    Pair(_, cdr) -> cdr
  | _ -> invalid_arg "Argument is not a Pair" ;;

let make_symbol name =
  try
    Hashtbl.find symbol_table name
  with Not_found ->
    let symbol = Symbol name
    in begin
      Hashtbl.add symbol_table name symbol;
      symbol
    end ;;

type environment =
    EmptyEnvironment
  | Environment of (lisp_object, lisp_object) Hashtbl.t * environment ;;

let extend_environment vars vals env =
  let rec frame = Hashtbl.create 5
  and aux vars vals =
    match vars with
      EmptyList -> ()
    | Pair(var, rest) -> begin
        Hashtbl.add frame var (car vals);
        aux rest (cdr vals)
    end
    | _ -> invalid_arg "Parameter `vars` must be type Pair"
  in begin
    aux vars vals;
    Environment(frame, env)
  end ;;

let setup_environment () =
  extend_environment EmptyList EmptyList EmptyEnvironment ;;

let first_frame = function
    EmptyEnvironment -> invalid_arg "Environment is empty already"
  | Environment(frame, _) -> frame ;;

let add_binding_to_frame var value frame =
  Hashtbl.add frame var value ;;

let rec lookup_variable_value var = function
    EmptyEnvironment -> failwith "Unbound variable"
  | Environment(frame, env) ->
      try
        Hashtbl.find frame var
      with Not_found -> lookup_variable_value var env ;;

let rec set_variable_value var value = function
    EmptyEnvironment -> failwith "Unbound variable"
  | Environment(frame, env) ->
      try
        ignore(Hashtbl.find frame var);
        Hashtbl.add frame var value
      with Not_found -> set_variable_value var value env ;;

(* read *)

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
      c when isspace c -> eat_whitespace input_stream
    | ';' -> let c = ref (getc input_stream)
    in begin
      while !c != '\n' do
        c := getc input_stream
      done;
      eat_whitespace input_stream
    end
    | c -> ungetc c input_stream
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
  then failwith "Character not followed by delimiter" ;;

let read_character in_stream =
  try
    let aux next rest result alt =
      if next = (peek in_stream)
      then begin
        eat_expected_string in_stream rest;
        peek_expected_delimiter in_stream;
        Character result
      end else begin
        peek_expected_delimiter in_stream;
        Character alt
      end
    in match (getc in_stream) with
    | 's' -> aux 'p' "pace" ' ' 's'
    | 'n' -> aux 'e' "ewline" '\n' 'n'
    | c -> Character c
  with Stream.Failure ->
    invalid_arg "Incomplete character literal\n" ;;

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
      else failwith "Number not followed by delimiter\n"
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

let quote_symbol = make_symbol "quote" ;;
let set_symbol = make_symbol "set!" ;;
let define_symbol = make_symbol "define" ;;
let ok_symbol = make_symbol "ok" ;;

let the_true = Boolean true ;;
let the_false = Boolean false ;;

(* mutually recursive: read_pair <-> read *)
let rec read_dotted_pair_cdr in_stream =
  match (peek in_stream) with
    c when not (isspace c) -> failwith "Dot not followed by whitespace"
  | _ -> begin
      eat_whitespace in_stream;
      let cdr_obj = read in_stream
      in begin
        eat_whitespace in_stream;
        match (getc in_stream) with
          ')' -> cdr_obj
        | _ -> failwith "Where was the trailing right paren?"
      end
  end

and read_pair in_stream =
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
        match (getc in_stream) with
          '.' -> Pair(car, read_dotted_pair_cdr in_stream)
        | c -> begin
            ungetc c in_stream;
            Pair(car, read_pair in_stream)
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
        | 't' -> the_true
        | 'f' -> the_false
        | '\\' -> read_character in_stream
        | _ -> failwith "Unknown boolean literal\n"
    end
    | '(' -> read_pair in_stream
    | '\'' -> Pair(quote_symbol, Pair(read in_stream, EmptyList))
    | c when isdigit c || (c = '-' && isdigit (peek in_stream)) ->
        read_fixnum in_stream c
    | c when is_double_quote c -> read_string in_stream
    | c when is_initial c || ((c = '+' || c = '-') && is_delimiter (peek in_stream)) -> read_symbol in_stream c
    | c -> (Printf.fprintf stderr "bad input. Unexpected '%c'\n" c; raise Exit)
  with End_of_file -> failwith "Read illegal state\n" ;;

(* eval *)

let is_self_evaluating = function
    Pair(_, _) | Symbol _ -> false
  | _ -> true ;;

let is_tagged_list exp tag =
  match exp with
    Pair(car, _) -> car = tag
  | _ -> false ;;

let is_quoted exp =
  is_tagged_list exp quote_symbol ;;

let text_of_quotation exp = car (cdr exp) ;;

let enclosing_environment = function
    EmptyEnvironment -> invalid_arg "Empty environment already\n"
  | Environment(_, env) -> env ;;

let is_symbol = function
    Symbol _ -> true
  | _ -> false ;;

let is_variable = is_symbol ;;

let define_variable var value = function
    EmptyEnvironment -> failwith "Empty environment.\n"
  | Environment(frame, _) -> add_binding_to_frame var value frame ;;

let is_assignment exp =
  is_tagged_list exp set_symbol ;;

let assignment_variable exp = car (cdr exp) ;;
let assignment_value exp = car (cdr (cdr exp)) ;;

let is_definition exp =
  is_tagged_list exp define_symbol ;;

let definition_variable exp = car (cdr exp) ;;
let definition_value exp = car (cdr (cdr exp)) ;;

(* mutually recursive: eval_assignment <-> eval *)
let rec eval_assignment exp env =
  begin
    set_variable_value
      (assignment_variable exp)
      (eval (assignment_value exp) env)
      env;
    ok_symbol
  end

and eval_definition exp env =
  begin
    define_variable
      (definition_variable exp)
      (eval (definition_value exp) env)
      env;
    ok_symbol
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
  | _ -> invalid_arg "Can not eval unknown expression type\n" ;;

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

let write_character = function
    '\n' -> print_string "#\\newline"
  | ' ' -> print_string "#\\space"
  | c -> Printf.printf "#\\%c" c ;;

(* mutually recursive: write_pair <-> write *)
let rec write_pair = function
    Pair(car, cdr) -> begin
      write car;
      match cdr with
        EmptyList -> ()
      | Pair(_, _) -> (print_char ' '; write_pair cdr)
      | _ -> (print_string " . "; write cdr)
    end
  | _ -> invalid_arg "Not a pair"

and write obj =
  match obj with
    Fixnum num -> Printf.printf "%d" num
  | Boolean true -> print_string "#t"
  | Boolean false -> print_string "#f"
  | Character c -> write_character c
  | EmptyList -> print_string "()"
  | Pair _ -> (print_char '('; write_pair obj; print_char ')')
  | Symbol name -> print_string name
  | String str -> write_string str ;;

(* toploop *)

let global_environment = setup_environment () ;;

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
