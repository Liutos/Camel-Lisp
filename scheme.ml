(* model *)

(* mutually recursive: lisp_object <-> environment *)
type lisp_object =
    Fixnum of int
  | Character of char
  | String of string
  | EmptyList
  | Pair of mutable_pair
  | Symbol of string
  | PrimitiveProc of (lisp_object -> lisp_object)
  | CompoundProc of lisp_object * lisp_object * environment
  | Boolean of bool

and mutable_pair = {mutable car: lisp_object; mutable cdr: lisp_object}

and environment =
    EmptyEnvironment
  | Environment of (lisp_object, lisp_object) Hashtbl.t * environment ;;

let car = function
    Pair pair -> pair.car
  | _ -> invalid_arg "Argument is not a Pair" ;;

let cdr = function
    Pair pair -> pair.cdr
  | _ -> invalid_arg "Argument is not a Pair" ;;

let cadr exp = car (cdr exp) ;;
let cddr exp = cdr (cdr exp) ;;
let caddr exp = car (cddr exp) ;;
let cdddr exp = cdr (cddr exp) ;;
let cadddr exp = car (cdddr exp) ;;

let make_pair a b =
  Pair {car = a; cdr = b} ;;

let symbol_table = Hashtbl.create 20 ;;

let make_symbol name =
  try
    Hashtbl.find symbol_table name
  with Not_found ->
    let symbol = Symbol name
    in begin
      Hashtbl.add symbol_table name symbol;
      symbol
    end ;;

let extend_environment vars vals env =
  let rec frame = Hashtbl.create 5
  and aux vars vals =
    match vars, vals with
    | EmptyList, EmptyList -> ()
    | Pair vars, Pair vals -> begin
        Hashtbl.add frame vars.car vals.car;
        aux vars.cdr vals.cdr
    end
    | _ -> invalid_arg "The arguments must be of type EmptyList or Pair"
  in begin
    aux vars vals;
    Environment(frame, env)
  end ;;

let setup_environment () =
  extend_environment EmptyList EmptyList EmptyEnvironment ;;

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

let getc in_channel =
  let stack = find_or_create_stack in_channel
  in if Stack.is_empty stack then
    input_char in_channel
  else Stack.pop stack ;;

let ungetc c in_channel =
  let stack = find_or_create_stack in_channel
  in Stack.push c stack ;;

let peek input_channel =
  let c = getc input_channel
  in (ungetc c input_channel; c) ;;

let isspace = function
    ' ' | '\n' | '\r' | '\t' -> true
  | _ -> false ;;

let isdigit = function
    '0'..'9' -> true
  | _ -> false ;;

let rec eat_comment input_channel =
  match (getc input_channel) with
  | '\n' -> ()
  | _ -> eat_comment input_channel ;;

let rec eat_whitespace input_channel =
  try
    match (getc input_channel) with
    | ';' -> begin
        eat_comment input_channel;
        eat_whitespace input_channel
    end
    | c when isspace c -> eat_whitespace input_channel
    | c -> ungetc c input_channel
  with End_of_file -> () ;;

let is_delimiter c =
  isspace c || c = '(' || c = ')' || c = '"' || c = ';' ;;

let error_char dir c =
  begin
    Printf.fprintf stderr dir c;
    flush stdout;
    raise Exit
  end ;;

let eat_expected_string in_channel str =
  let aux c =
    if c != (getc in_channel) then
      error_char "Unexpcted character '%c'\n" c
  in String.iter aux str ;;

let peek_expected_delimiter in_channel =
  let c = peek in_channel
  in if not (is_delimiter c) then
    failwith "Character not followed by delimiter" ;;

let read_character in_channel =
  try
    let aux next rest result alt =
      if next = (peek in_channel) then
        begin
          eat_expected_string in_channel rest;
          peek_expected_delimiter in_channel;
          Character result
        end
      else begin
        peek_expected_delimiter in_channel;
        Character alt
      end
    in match (getc in_channel) with
    | 's' -> aux 'p' "pace" ' ' 's'
    | 'n' -> aux 'e' "ewline" '\n' 'n'
    | c -> Character c
  with End_of_file ->
    invalid_arg "Incomplete character literal" ;;

let read_string in_channel =
  let rec buf = Buffer.create 80
  and aux () =
    match (getc in_channel) with
    | '"' -> String (Buffer.contents buf)
    | '\\' -> begin
        match (getc in_channel) with
        | 'n' -> (Buffer.add_char buf '\n'; aux ())
        | c -> (Buffer.add_char buf c; aux ())
    end
    | c -> (Buffer.add_char buf c; aux ())
  in aux () ;;

let read_fixnum in_channel c =
  let sign = if c = '-' then -1 else (ungetc c in_channel; 1)
  and num = ref 0
  in try
    let c = ref (getc in_channel)
    in begin
      while isdigit !c do
        num := !num * 10 + (Char.code !c) - (Char.code '0');
        c := getc in_channel
      done;
      num := !num * sign;
      if is_delimiter !c then
        (ungetc !c in_channel; Fixnum !num)
      else failwith "Number not followed by delimiter"
    end
  with End_of_file -> Fixnum !num ;;

let is_initial = function
    'a'..'z' | 'A'..'Z' | '*' | '/' | '>' | '<' | '=' | '?' | '!' -> true
  | _ -> false ;;

let read_symbol in_channel init =
  let buf = Buffer.create 10
  in let c = ref init
  in begin
    while is_initial !c || isdigit !c || !c = '+' || !c = '-' do
      Buffer.add_char buf !c;
      c := getc in_channel
    done;
    if is_delimiter !c then
      (ungetc !c in_channel; make_symbol (Buffer.contents buf))
    else error_char "Symbol not followed by delimiter. Found '%c'\n" !c
  end ;;

let quote_symbol = make_symbol "quote" ;;
let set_symbol = make_symbol "set!" ;;
let define_symbol = make_symbol "define" ;;
let ok_symbol = make_symbol "ok" ;;
let if_symbol = make_symbol "if" ;;
let lambda_symbol = make_symbol "lambda" ;;
let begin_symbol = make_symbol "begin" ;;
let cond_symbol = make_symbol "cond" ;;
let else_symbol = make_symbol "else" ;;

let the_true = Boolean true ;;
let the_false = Boolean false ;;

(* mutually recursive: read_pair <-> read *)
let rec read_dotted_pair_cdr in_channel =
  match (peek in_channel) with
  | c when not (isspace c) -> failwith "Dot not followed by whitespace"
  | _ -> begin
      eat_whitespace in_channel;
      let cdr_obj = read in_channel
      in begin
        eat_whitespace in_channel;
        match (getc in_channel) with
        | ')' -> cdr_obj
        | _ -> failwith "Where was the trailing right paren?"
      end
  end

and read_pair in_channel =
  begin
    eat_whitespace in_channel;
    let c = ref (getc in_channel)
    in if !c = ')' then
      EmptyList
    else begin
      ungetc !c in_channel;
      let car = read in_channel
      in begin
        eat_whitespace in_channel;
        match (getc in_channel) with
        | '.' -> make_pair car (read_dotted_pair_cdr in_channel)
        | c -> begin
            ungetc c in_channel;
            make_pair car (read_pair in_channel)
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
        | 't' -> the_true
        | 'f' -> the_false
        | '\\' -> read_character in_channel
        | _ -> failwith "Unknown boolean literal"
    end
    | '(' -> read_pair in_channel
    | '\'' ->
        make_pair
          quote_symbol
          (make_pair (read in_channel) EmptyList)
    | c when isdigit c || (c = '-' && isdigit (peek in_channel)) ->
        read_fixnum in_channel c
    | '"' -> read_string in_channel
    | c when is_initial c || ((c = '+' || c = '-') && is_delimiter (peek in_channel)) -> read_symbol in_channel c
    | c -> error_char "Bad input. Unexpected '%c'\n" c
  with End_of_file -> failwith "Read illegal state" ;;

(* eval *)

let is_self_evaluating = function
    Pair _ | Symbol _ -> false
  | _ -> true ;;

let is_tagged_list exp tag =
  match exp with
  | Pair {car; _} -> car = tag
  | _ -> false ;;

let is_quoted exp =
  is_tagged_list exp quote_symbol ;;

let text_of_quotation = cadr ;;

let is_symbol = function
    Symbol _ -> true
  | _ -> false ;;

let is_variable = is_symbol ;;

let define_variable var value = function
    EmptyEnvironment -> invalid_arg "Empty environment"
  | Environment(frame, _) -> add_binding_to_frame var value frame ;;

let is_assignment exp =
  is_tagged_list exp set_symbol ;;

let assignment_variable = cadr ;;
let assignment_value = caddr ;;

let is_definition exp =
  is_tagged_list exp define_symbol ;;

let definition_variable exp =
  match (cadr exp) with
  | Symbol _ -> cadr exp
  | Pair obj -> obj.car
  | _ -> invalid_arg "Argument is not of type Pair" ;;

let make_lambda params body =
  make_pair lambda_symbol (make_pair params body) ;;

let definition_value exp =
  match (cadr exp) with
  | Symbol _ -> caddr exp
  | Pair _ -> make_lambda (cdr (cadr exp)) (cdr (cdr exp))
  | _ -> invalid_arg "Argument is not of type Pair" ;;

let is_if exp =
  is_tagged_list exp if_symbol ;;

let if_predicate = cadr ;;
let if_consequent = caddr ;;

let if_alternative exp =
  match (cdddr exp) with
  | EmptyList -> the_false
  | alt -> car alt ;;

let is_true obj =
   obj != the_false ;;

let is_application = function
  | Pair _ -> true
  | _ -> false ;;

let operator = car ;;
let operands = cdr ;;

let is_empty_list = function
  | EmptyList -> true
  | _ -> false ;;

let is_no_operands = is_empty_list ;;
let first_operand = car ;;
let rest_operands = cdr ;;

(* let fn_value = function *)
(*   | PrimitiveProc fn -> fn *)
(*   | _ -> invalid_arg "Argument is not of type PrimitiveProc" ;; *)

let is_lambda exp =
  is_tagged_list exp lambda_symbol ;;

let lambda_parameters = cadr ;;
let lambda_body exp = cdr (cdr exp) ;;

let eval_lambda exp env =
  let params = lambda_parameters exp
  and body = lambda_body exp
  in CompoundProc(params, body, env) ;;

let is_begin exp =
  is_tagged_list exp begin_symbol ;;

let begin_action = cdr ;;

let make_if predicate consequent alternative =
  make_pair if_symbol
      (make_pair predicate
         (make_pair consequent
            (make_pair alternative EmptyList))) ;;

let make_begin seq =
  make_pair begin_symbol seq ;;

let is_cond exp =
  is_tagged_list exp cond_symbol ;;

let cond_clauses = cdr ;;
let cond_predicate = car ;;
let cond_action = cdr ;;
let is_cond_else_clauses clauses =
  cond_predicate clauses = else_symbol ;;

let sequence_to_exp = function
    EmptyList -> EmptyList
  | Pair {car; cdr = EmptyList} -> car
  | seq -> make_begin seq ;;

let rec expand_clauses = function
    EmptyList -> the_false
  | Pair {car; cdr} -> begin
      if is_cond_else_clauses car then
        if cdr = EmptyList then
          sequence_to_exp (cond_action car)
        else begin
          prerr_string "else clause isn't last cond->if\n";
          raise Exit
        end
      else make_if
             (cond_predicate car)
             (sequence_to_exp (cond_action car))
             (expand_clauses cdr)
  end
  | _ -> invalid_arg "Argument is not of type EmptyList or Pair" ;;

let cond_to_if exp =
  expand_clauses (cond_clauses exp) ;;

(* mutually recursive: eval_assignment <-> eval <-> eval_definition *)
let rec list_of_values exps env =
  if is_no_operands exps then
    EmptyList
  else
    make_pair
      (eval (first_operand exps) env)
      (list_of_values (rest_operands exps) env)

and eval_assignment exp env =
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

and eval_if exp env =
  let result =
    if is_true (eval (if_predicate exp) env) then
      if_consequent exp
    else if_alternative exp
  in eval result env

and eval_begin exp env =
  let rec aux = function
      Pair {car = exp; cdr = EmptyList} -> eval exp env
    | Pair {car = first; cdr = rest} -> begin
        ignore(eval first env);
        aux rest
    end
    | _ -> invalid_arg "Impossible"
  in aux (begin_action exp)

and eval exp env =
  match exp with
  | exp when is_self_evaluating exp -> exp
  | exp when is_variable exp -> lookup_variable_value exp env
  | exp when is_assignment exp -> eval_assignment exp env
  | exp when is_definition exp -> eval_definition exp env
  | exp when is_quoted exp -> text_of_quotation exp
  | exp when is_if exp -> eval_if exp env
  | exp when is_lambda exp -> eval_lambda exp env
  | exp when is_begin exp -> eval_begin exp env
  | exp when is_cond exp -> eval (cond_to_if exp) env
  | exp when is_application exp -> begin
      let procedure = eval (operator exp) env
      and arguments = list_of_values (operands exp) env
      in match procedure with
      | PrimitiveProc fn -> fn arguments
      | CompoundProc(params, body, env) ->
          let rec new_env = extend_environment params arguments env
          and aux = function
              Pair {car = exp; cdr = EmptyList} -> eval exp new_env
            | Pair {car = exp; cdr = rest} -> begin
                ignore(eval exp new_env);
                aux rest
            end
            | _ -> invalid_arg "Function body is not of type Pair"
          in aux body
      | _ -> invalid_arg "Unknown procedure type"
  end
  | _ -> invalid_arg "Can not eval unknown expression type" ;;

(* print *)

let write_string str =
  begin
    print_char '"';
    String.iter
      (fun c ->
        match c with
        | '\n' -> print_string "\\n"
        | '\\' -> print_string "\\\\"
        | c when c = '"' -> print_string "\\\""
        | c -> print_char c)
      str;
    print_char '"'
  end ;;

let write_character = function
    '\n' -> print_string "#\\newline"
  | ' ' -> print_string "#\\space"
  | c -> Printf.printf "#\\%c" c ;;

(* mutually recursive: write_pair <-> write *)
let rec write_pair = function
    Pair {car; cdr} -> begin
      write car;
      match cdr with
      | EmptyList -> ()
      | Pair _ -> (print_char ' '; write_pair cdr)
      | _ -> (print_string " . "; write cdr)
    end
  | _ -> invalid_arg "Not a pair"

and write obj =
  match obj with
  | Fixnum num -> Printf.printf "%d" num
  | Boolean true -> print_string "#t"
  | Boolean false -> print_string "#f"
  | Character c -> write_character c
  | EmptyList -> print_string "()"
  | Pair _ -> (print_char '('; write_pair obj; print_char ')')
  | Symbol name -> print_string name
  | PrimitiveProc _ | CompoundProc _ -> print_string "#<procedure>"
  | String str -> write_string str ;;

(* toploop *)

let global_environment = setup_environment () ;;

(* arithmetic operations *)

let add_proc args =
  let rec aux = function
      EmptyList -> 0
    | Pair {car = Fixnum n; cdr = ns} -> n + aux ns
    | _ -> invalid_arg "All arguments must be of type fixnum"
  in Fixnum (aux args) ;;

let sub_proc args =
  let rec aux acc rest =
    match rest with
    | EmptyList -> acc
    | Pair {car = Fixnum n; cdr = ns} -> aux (acc - n) ns
    | _ -> invalid_arg "All arguments must be of type fixnum"
  in match args with
  | EmptyList -> Fixnum 0
  | Pair {car = Fixnum n; cdr = rest} -> Fixnum (aux n rest)
  | _ -> invalid_arg "Argument must be of type Pair" ;;

let mul_proc args =
  let rec aux = function
      EmptyList -> 1
    | Pair {car = Fixnum n; cdr = ns} -> n * aux ns
    | _ -> invalid_arg "Argument must be of type Fixnum"
  in Fixnum (aux args) ;;

let quotient_proc args =
  match args with
  | Pair {car = Fixnum a; cdr = Pair {car = Fixnum b; cdr = EmptyList}} ->
      Fixnum (a / b)
  | _ -> invalid_arg "Argument muse be a proper list contains two fixnums" ;;

let remainder_proc args =
  match args with
  | Pair {car = Fixnum a; cdr = Pair {car = Fixnum b; cdr = EmptyList}} ->
      Fixnum (a mod b)
  | _ -> invalid_arg "Argument muse be a proper list contains two fixnums" ;;

let is_number_equal_proc args =
  let rec aux cur rest =
    match rest with
    | EmptyList -> the_true
    | Pair {car = Fixnum n; cdr = ns} ->
        if cur = n then
          aux n ns
        else the_false
    | _ -> invalid_arg "Argument must be of type Fixnum"
  in match args with
  | Pair {car = Fixnum cur; cdr = rest} -> aux cur rest
  | _ ->
      invalid_arg "Argument must be a proper list contains at least one fixnum" ;;

let is_less_than_proc args =
  let rec aux cur rest =
    match rest with
    | EmptyList -> the_true
    | Pair {car = Fixnum n; cdr = ns} ->
        if cur < n then
          aux n ns
        else the_false
    | _ -> invalid_arg "Argument must be of type Fixnum"
  in match args with
  | Pair {car = Fixnum cur; cdr = rest} -> aux cur rest
  | _ ->
      invalid_arg "Argument must be a proper list contains at least one fixnum" ;;

let is_greater_than_proc args =
  let rec aux cur rest =
    match rest with
    | EmptyList -> the_true
    | Pair {car = Fixnum n; cdr = ns} ->
        if cur > n then
          aux n ns
        else the_false
    | _ -> invalid_arg "Argument must be of type Fixnum"
  in match args with
  | Pair {car = Fixnum cur; cdr = rest} -> aux cur rest
  | _ ->
      invalid_arg "Argument must be a proper list contains at least one fixnum" ;;

(* list operations *)

let cons_proc = function
    Pair {car; cdr = Pair {cdr = obj; _}} -> make_pair car obj
  | _ -> invalid_arg "Arguments must be a proper list contains two objects" ;;

let car_proc = function
    Pair {car = Pair {car; _}; cdr = EmptyList} -> car
  | _ -> invalid_arg "Arguments must be a proper list contains one Pair" ;;

let cdr_proc = function
    Pair {car = Pair {car = _; cdr}; cdr = EmptyList} -> cdr
  | _ -> invalid_arg "Arguments must be a proper list contains one Pair" ;;

let set_car_proc = function
    Pair {car = Pair obj; cdr = value} -> (obj.car <- car value; ok_symbol)
  | _ -> invalid_arg "Arguments must be a proper list contains two objects" ;;

let set_cdr_proc = function
    Pair {car = Pair obj; cdr = value} -> (obj.cdr <- cdr value; ok_symbol)
  | _ -> invalid_arg "Arguments must be a proper list contains two objects" ;;

(* type predicates *)

let is_null_proc = function
    Pair {car = EmptyList; _} -> the_true
  | _ -> the_false ;;

let is_boolean_proc = function
    Pair {car = Boolean _; _} -> the_true
  | _ -> the_false ;;

let is_symbol_proc = function
    Pair {car = Symbol _; _} -> the_true
  | _ -> the_false ;;

let is_integer_proc = function
    Pair {car = Fixnum _; _} -> the_true
  | _ -> the_false ;;

let is_char_proc = function
    Pair {car = Character _; _} -> the_true
  | _ -> the_false ;;

let is_string_proc = function
    Pair {car = String _; _} -> the_true
  | _ -> the_false ;;

let is_pair_proc = function
    Pair {car = Pair _; _} -> the_true
  | _ -> the_false ;;

(* type conversions *)

let char_to_integer_proc = function
    Pair {car = Character c; _} -> Fixnum (Char.code c)
  | _ -> invalid_arg "Argument is not of type Character" ;;

let integer_to_char_proc = function
    Pair {car = Fixnum num; _} -> Character (Char.chr num)
  | _ -> invalid_arg "Argument is not of type Fixnum" ;;

let number_to_string_proc = function
    Pair {car = Fixnum num; _} -> String (string_of_int num)
  | _ -> invalid_arg "Argument is not of type Fixnum" ;;

let string_to_number_proc = function
    Pair {car = String str; _} -> Fixnum (int_of_string str)
  | _ -> invalid_arg "Argument is not of type String" ;;

let symbol_to_string_proc = function
    Pair {car = Symbol name; _} -> String name
  | _ -> invalid_arg "Argument is not of type Symbol" ;;

let string_to_symbol_proc = function
    Pair {car = String str; _} -> make_symbol str
  | _ -> invalid_arg "Argument is not of type String" ;;

let add_primitive_procedure name fn =
  define_variable (make_symbol name) (PrimitiveProc fn) global_environment ;;

let init () =
  let kvs =
    [("+", add_proc);
     ("null?", is_null_proc);
     ("boolean?", is_boolean_proc);
     ("symbol?", is_symbol_proc);
     ("integer?", is_integer_proc);
     ("char?", is_char_proc);
     ("string?", is_string_proc);
     ("pair?", is_pair_proc);
     ("char->integer", char_to_integer_proc);
     ("integer->char", integer_to_char_proc);
     ("number->string", number_to_string_proc);
     ("string->number", string_to_symbol_proc);
     ("symbol->string", symbol_to_string_proc);
     ("string->symbol", string_to_symbol_proc);
     ("-", sub_proc);
     ("*", mul_proc);
     ("quotient", quotient_proc);
     ("remainder", remainder_proc);
     ("=", is_number_equal_proc);
     ("<", is_less_than_proc);
     (">", is_greater_than_proc);
     ("cons", cons_proc);
     ("car", car_proc);
     ("cdr", cdr_proc);
     ("set-car!", set_car_proc);
     ("set-cdr!", set_cdr_proc)]
  in List.iter
    (fun (name, fn) -> add_primitive_procedure name fn)
    kvs ;;

let main () =
  begin
    init ();
    print_string "Welcome to Bootstrap Scheme. Use ctrl-c to exit.\n";
    flush stdout;
    while true do
      print_string "> ";
      flush stdout;
      write (eval (read stdin) global_environment);
      print_newline ()
    done;
    0
  end ;;

let rep () =
  write (eval (read stdin) global_environment) ;;
