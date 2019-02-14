open Stdlib

type operator = Plus | Minus | Mult | Div | Power | None
exception Invalid_RPN_Statement

type expr =
  | Var of float
  | Add of float * float
  | Sub of float * float
  | Mul of float * float
  | Div of float * float
  | Pwr of float * float

let eval(x: expr) = 
  match x with
    | Var(e) -> e
    | Add(a, b) -> b+.a
    | Sub(a, b) -> b-.a
    | Mul(a, b) -> b*.a
    | Div(a, b) -> b/.a
    | Pwr(a, b) -> b**a

let result(stack) = 
  match stack with
    | hd :: [] -> hd
    | hd :: tl -> raise Invalid_RPN_Statement
    | _ -> raise Invalid_RPN_Statement

let stack_mod (op: operator)(stack: float list) = 
  match stack, op with
    | o1::o2::tl, Plus -> eval(Add(o1,o2))::tl
    | o1::o2::tl, Minus -> eval(Sub(o1,o2))::tl
    | o1::o2::tl, Mult -> eval(Mul(o1,o2))::tl
    | o1::o2::tl, Div -> eval(Div(o1,o2))::tl
    | o1::o2::tl, Power -> eval(Pwr(o1,o2))::tl
    | o1 :: [], op -> raise Invalid_RPN_Statement 
    | _ -> []


let rec traverse(li, stack) =
  match li with
    | "+" :: tl -> traverse(tl, stack_mod(Plus)(stack))
    | "-" :: tl -> traverse(tl, stack_mod(Minus)(stack))
    | "*" :: tl -> traverse(tl, stack_mod(Mult)(stack))
    | "/" :: tl -> traverse(tl, stack_mod(Div)(stack))
    | "^" :: tl -> traverse(tl, stack_mod(Power)(stack))
    | float :: tl -> traverse(tl, float_of_string(float)::stack)
    | [] -> result(stack);;
let in_channel = open_in "tests.txt" in
try
  while true do
    let line = input_line in_channel in
    traverse (Str.split (Str.regexp " +") line, [])
    (* do something with line *)
  done
with End_of_file ->
  close_in in_channel