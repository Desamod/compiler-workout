open GT       
open Language
       
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
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
 let eval_config config insn =
	let (stack, st_cfg) = config in
	let (state, istream, ostream) = st_cfg in match insn with
	    | BINOP operator -> (match stack with
		    | y::x::tail -> ([(Language.Expr.operation operator) x y] @ tail, st_cfg))

           | CONST value -> ([value] @ stack, st_cfg)

	    | READ -> (match istream with
		    | head::tail -> ([head] @ stack, (state, tail, ostream)))
	    | WRITE -> (match stack with
		    | head::tail -> (tail, (state, istream, ostream @ [head])))

	    | LD  var -> ([state var] @ stack, st_cfg)
	    | ST  var -> (match stack with
		    | head::tail -> (tail, (Language.Expr.update var head state, istream, ostream)))
 
let eval config prg = List.fold_left eval_config config prg

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Language.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)
let rec compile_expr expr = match expr with
       | Language.Expr.Const  const         -> [CONST const]
       | Language.Expr.Var    var         -> [LD var]
       | Language.Expr.Binop (op, l, r) -> (comp_expression l) @ (comp_expression r) @ [BINOP op]

let rec compile stmt = match stmt with
       | Language.Stmt.Read    var       -> [READ; ST var]
       | Language.Stmt.Write   expr       -> (compile_expr expr) @ [WRITE]
       | Language.Stmt.Assign (var, expr)   -> (compile_expr expr) @ [ST var]
       | Language.Stmt.Seq    (l, r) -> (compile l) @ (compile r)
