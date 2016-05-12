open System

type Do = Do
(*let (<<) int1 int2 = isSmaller(int1,int2)
let (>>) int1 int2 = isBigger(int1, int2)                       //some extra possible ways to compare integers
let (:=) int1 int2 = isEquals(int1, int2
*)

type pairOfFloats = {
                        firstFloat:float;
                        secondFloat:float;
                        printSomething:bool
                    } 

type Expr =
    | Integer of int
    | String of string
    | Boolean of bool

type Instruction =
    | PrintLine of Expr
    | Add of Expr * Expr
    | Subtract of Expr * Expr
    | Assign of string * Expr
    | Sequence of Instruction * Instruction
    | While of Expr * Instruction
 //   | For of Expr * Expr * Expr * Instruction

let rec eval (e:Expr) : Expr =
  match e with
  | Integer i -> Integer i
  | Boolean b -> Boolean b
  | String x -> String x

let checkInt (e:Expr) : bool =
    match e with 
    | String s -> false
    | Integer i -> true
    | Boolean b -> false

let parseInt (e:Expr) : int =
    match e with
    | Integer i -> i



//let doublePair pair:pairOfInts =
//    let pair = {integer1 = pair.integer1*2 ; integer2 = pair.integer2*2}
 //   printfn "%A" pair

let average (int1,int2) = //calculate average of 2 ints
    printfn "%A" ((int1+int2)/2)

let averageFloat (float1,float2,printSomethingBoolean) = //if true -> print result else print nothing
    let pairOfFloats = {firstFloat = float1 ; secondFloat = float2; printSomething = printSomethingBoolean}
    match printSomethingBoolean with 
        | true -> printfn "%A" ((pairOfFloats.firstFloat + pairOfFloats.secondFloat) / 2.0)
        | false -> ()


let rec _while (e:Expr) (instr:Instruction) =
            match eval e with
                | Boolean true -> 
                 _while e instr                      //run should still be called
                | _ -> printfn "Break"

let rec run (i:Instruction) =
    match i with
    | PrintLine(x) ->
      printfn "%A" x
    | Add(int1,int2) ->                                 //add takes two integers and sums them, throws error if types are not 2 ints
      if checkInt int1 && checkInt int2 then 
         let in1 = parseInt(int1) 
         let in2 = parseInt(int2)
         let result = in1+in2
         printfn "%i" result 
      else printfn "Error: Add() takes two integers"
    | Subtract(int1,int2) ->
      if checkInt int1 && checkInt int2 then 
         let in1 = parseInt(int1) 
         let in2 = parseInt(int2)
         let result = in1-in2
         printfn "%i" result 
      else printfn "Error: Subtract() takes two integers"
    | Sequence(s1,s2) ->
      run s1
      run s2
   (* | For(expr, expr2, expr3, instr) ->                                  //for not complete yet
      if checkInt expr && checkInt expr2 && checkInt expr3 then
         let intStart = parseInt(expr)
         let intLimit = parseInt(expr2)
         let intIncrement = parseInt(expr3)
         
         if intStart < intLimit then
            let result = intStart + intIncrement
            printfn "%A" result *)
     (* | While (expr,instr) ->                                           //while is recursive but doesn't run an instruction
        _while expr instr*)
         
        
let __while expr (_:Do) instr = While(expr,instr)
let _do = Do
let (++) int1 int2 = Add(int1,int2)   
let (--) int1 int2 = Subtract(int1,int2)    
let (><) s1 s2 = Sequence(s1,s2)
let program = PrintLine(String "Foo") >< 
              PrintLine(String "Bar") ><
              PrintLine(Integer 2) ><
              Integer 5 ++ String "p" ><
              Integer 5 ++ Integer 6 ><
              Integer 6 -- Integer 7


run program

let avg = average(10,20)
let avgFloat = averageFloat(8.43,8.2,true)
let unused = Console.ReadLine()



