open System

type Then = Then
type Else = Else

type Expr =
    | Integer of int
    | String of string
    | Boolean of bool

type Instruction =
    | PrintLine of Expr
    | Add of Expr * Expr
    | Assign of string * Expr
    | Sequence of Instruction * Instruction

let sameType(expr1: Expr) (expr2: Expr) : bool = 
    match expr1 with 
    | expr2 -> true
    | _ -> false

let checkInt (e:Expr) : bool =
    match e with 
    | String s -> false
    | Integer i -> true
    | Boolean b -> false

let parseInt (e:Expr) : int =
    match e with
    | Integer i -> i

let rec run (i:Instruction) =
    match i with
    | PrintLine(x) ->
      printfn "%A" x
    | Add(int1,int2) -> 
      if checkInt int1 && checkInt int2 then 
         let in1 = parseInt(int1) 
         let in2 = parseInt(int2)
         let result = in1+in2
         printfn "%i" result 
      else printfn "Error: Add() takes two integers"
    | Sequence(s1,s2) ->
      run s1
      run s2

let (><) s1 s2 = Sequence(s1,s2)
let program = PrintLine(String "Foo") >< 
              PrintLine(String "Bar") ><
              PrintLine(Integer 2) ><
              Add(Integer 5, String 6)

run program
let unused = Console.ReadLine()

