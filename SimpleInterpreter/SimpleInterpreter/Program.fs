open System

(*let (<<) int1 int2 = isSmaller(int1,int2)
let (>>) int1 int2 = isBigger(int1, int2)
let (:=) int1 int2 = isEquals(int1, int2)*)

type Expr =
    | Integer of int
    | String of string
    | Boolean of bool

type Instruction =
    | PrintLine of Expr
    | Add of Expr * Expr
    | Assign of string * Expr
    | Sequence of Instruction * Instruction
//    | While of Expr * Instruction
    | For of Expr * Expr * Expr * Instruction



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
    | For(expr, expr2, expr3, instr) -> 
      if checkInt expr && checkInt expr2 && checkInt expr3 then
         let intStart = parseInt(expr)
         let intLimit = parseInt(expr2)
         let intIncrement = parseInt(expr3)
         
         if intStart < intLimit then
            let result = intStart + intIncrement
            printfn "%A" result


    (*| While(bool,instr) -> 
      if !bool then
        run instr
      else if bool then
        run instr *)
        
let (++) int1 int2 = Add(int1,int2)       
let (><) s1 s2 = Sequence(s1,s2)
let _for expr1 expr2 expr3 instr = For(expr1, expr2, expr3 instr)
// let _while expr instr = While(expr,instr)
let program = PrintLine(String "Foo") >< 
              PrintLine(String "Bar") ><
              PrintLine(Integer 2) ><
              Integer 5 ++ Integer 8


run program
let unused = Console.ReadLine()

