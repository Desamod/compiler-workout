(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT 
    
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)
let operation arg left right = 
  match arg with
  | "+"  -> left + right
  | "-"  -> left - right
  | "*"  -> left * right
  | "/"  -> left / right
  | "%"  -> left mod right
  | ">"  -> if (left > right) then 1 else 0 
  | "<"  -> if (left < right) then 1 else 0 
  | "==" -> if (left = right) then 1 else 0
  | ">=" -> if (left >= right) then 1 else 0 
  | "<=" -> if (left <= right) then 1 else 0 
  | "!=" -> if (left != right) then 1 else 0 
  | "!!" -> if (left != 0 || right != 0) then 1 else 0 
  | "&&" -> if (left != 0 && right != 0) then 1 else 0 

  
let rec eval stat expr = 
  match expr with
  | Const numb -> numb
  | Var f -> stat f
  | Binop (arg, left, right) -> operation arg (eval stat left) (eval stat right)


  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
      let rec eval config stmt =
        let (state, istream, ostream) = config in
        match stmt with
        | Read var -> (
            match istream with
            | input::istream ->
                let state = Expr.update var input state in
                (state, istream, ostream)
            | [] -> failwith "Empty input stream")
        | Write expr ->
            let output = Expr.eval state expr in
            (state, istream, ostream @ [output])
        | Assign (var, expr) ->
            let res = Expr.eval state expr in
            (Expr.update var res state, istream, ostream)
        | Seq (l, r) ->
            eval (eval config l) r
                                                         
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : int list -> t -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval i p =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o
