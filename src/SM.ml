open GT       
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Syntax.Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
let eval config code =
    let (stack, (state, istream, ostream)) = config in
    let rec eval' (stack, (state, istream, ostream)) code =
        match code with
        | [] -> (stack, (state, istream, ostream))
        | instruction::code' ->
            eval' (match instruction with
                | BINOP op ->
                    let l::r::stack' = stack in
                    ((Syntax.Expr.eval_binop op l r)::stack', (state, istream, ostream))
                | CONST const ->
                    (const::stack, (state, istream, ostream))
                | READ ->
                    let value::istream' = istream in
                    (value::stack, (state, istream', ostream))
                | WRITE ->
                    let value::stack' = stack in
                    (stack', (state, istream, ostream @ [value]))
                | LD var ->
                    let value = state var in
                    (value::stack, (state, istream, ostream))
                | ST var ->
                    let value::stack' = stack in
                    (stack', ((Syntax.Expr.update var value state), istream, ostream))
                | _ -> failwith "Invalid instruction"
            ) code' in
    eval' (stack, (state, istream, ostream)) code

(* Top-level evaluation

     val run : int list -> prg -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Syntax.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let rec compile_expr expr =
  match expr with
  | Syntax.Expr.Const const ->
    [CONST const]
  | Syntax.Expr.Var var ->
    [LD var]
  | Syntax.Expr.Binop (op, l, r) ->
    (compile_expr r) @ (compile_expr l) @ [BINOP op]

let rec compile stmt =
  match stmt with
  | Syntax.Stmt.Read var ->
    [READ; ST var]
  | Syntax.Stmt.Write expr ->
    (compile_expr expr) @ [WRITE]
  | Syntax.Stmt.Assign (var, expr) ->
    (compile_expr expr) @ [ST var]
  | Syntax.Stmt.Seq (l, r) ->
    (compile l) @ (compile r)
