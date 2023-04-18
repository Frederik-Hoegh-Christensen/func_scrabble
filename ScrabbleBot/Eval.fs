// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval
    open StateMonad
    //open Types
    open ScrabbleUtil

    (* Code for testing *)

    let hello = [('H',4); ('E',1);('L',1);('L',1);('O',1)]
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    
    let add a b : SM<int> = a >>= fun c -> b >>= fun d -> ret(c+d) 

    let div a b = a >>= fun c -> b >>= fun d -> if d = 0 then fail(DivisionByZero) else ret(c/d)  

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    //type coord  = int * int 
    type word   = (char * int) list
    
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun = coord -> Result<square option, Error>
    
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
    }
    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    


    

    let rec arithEval a : SM<int> = a|> function 
           |N n-> ret n 
           |V v -> lookup v 
           |WL -> wordLength 
           |PV p -> arithEval p >>= fun f -> pointValue f
           |Add (a,b) -> arithEval a >>= fun f -> arithEval b >>= fun s -> f+s|>ret  
           |Sub (a,b)-> arithEval a >>= fun f -> arithEval b >>= fun s -> f-s |> ret  
           |Mul(a,b)-> arithEval a >>= fun f -> arithEval b >>= fun s -> f*s|> ret   
           |Div(a,b) -> arithEval a >>= fun f -> arithEval b >>= fun s -> f/s|> ret  
           |Mod (a,b) -> arithEval b >>= fun s -> if s=0 then fail DivisionByZero else arithEval a >>= fun f -> f%s|> ret
           |CharToInt c -> charEval c >>=  fun f ->  int f|>ret

        

    and charEval c : SM<char> = c|> function 
            |C c ->  ret c 
            |CV c -> arithEval c >>= fun f -> characterValue f 
            |ToUpper c -> charEval c >>= fun f -> f|> System.Char.ToUpper |> ret 
            |ToLower c -> charEval c >>= fun f ->  f|> System.Char.ToLower|> ret  
            |IntToChar i -> arithEval i >>= fun f -> char f|> ret  
            



    let rec boolEval b : SM<bool> = b|> function
           |TT -> ret true  
           |FF -> ret false 
           |AEq (a,b) ->  arithEval a >>= fun f -> arithEval b >>= fun s -> f=s |> ret  
           |ALt (a,b) -> arithEval a >>= fun f -> arithEval b >>= fun s -> f<s |> ret  
           |Not b -> boolEval b >>= fun f -> not f |> ret 
           |Conj(a,b) ->  boolEval a >>= fun f -> boolEval b >>= fun s -> (f && s) |> ret 
           |IsVowel c -> charEval c >>= fun f -> "aeoiuåæøAEOIUÅÆØ".Contains f |> ret 
           |IsLetter c -> charEval c >>= fun f -> System.Char.IsLetter f |> ret 
           |IsDigit c -> charEval c >>= fun f -> System.Char.IsDigit f |> ret 



    type stmnt =                  (* statements *)
    | Declare of string           (* variable declaration *)
    | Ass of string * aExp        (* variable assignment *)
    | Skip                        (* nop *)
    | Seq of stmnt * stmnt        (* sequential composition *)
    | ITE of bExp * stmnt * stmnt (* if-then-else statement *)
    | While of bExp * stmnt       (* while statement *)

    let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 (Optional) *) 

    let stmntToSquareFun stm = failwith "Not implemented"

    let stmntToBoardFun stm m = failwith "Not implemented"

    type squareStmnt = Map<int, stmnt>
    let stmntsToSquare stms = failwith "Not implemented"

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"

//    open StateMonad

//    let add a b = failwith "Not implemented"      
//    let div a b = failwith "Not implemented"      

//    type aExp =
//        | N of int
//        | V of string
//        | WL
//        | PV of aExp
//        | Add of aExp * aExp
//        | Sub of aExp * aExp
//        | Mul of aExp * aExp
//        | Div of aExp * aExp
//        | Mod of aExp * aExp
//        | CharToInt of cExp

//    and cExp =
//       | C  of char  (* Character value *)
//       | CV of aExp  (* Character lookup at word index *)
//       | ToUpper of cExp
//       | ToLower of cExp
//       | IntToChar of aExp

//    type bExp =             
//       | TT                   (* true *)
//       | FF                   (* false *)

//       | AEq of aExp * aExp   (* numeric equality *)
//       | ALt of aExp * aExp   (* numeric less than *)

//       | Not of bExp          (* boolean not *)
//       | Conj of bExp * bExp  (* boolean conjunction *)

//       | IsVowel of cExp      (* check for vowel *)
//       | IsConsonant of cExp  (* check for constant *)

//    let (.+.) a b = Add (a, b)
//    let (.-.) a b = Sub (a, b)
//    let (.*.) a b = Mul (a, b)
//    let (./.) a b = Div (a, b)
//    let (.%.) a b = Mod (a, b)

//    let (~~) b = Not b
//    let (.&&.) b1 b2 = Conj (b1, b2)
//    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
//    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
//    let (.=.) a b = AEq (a, b)   
//    let (.<.) a b = ALt (a, b)   
//    let (.<>.) a b = ~~(a .=. b)
//    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
//    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
//    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

//    let arithEval a : SM<int> = failwith "Not implemented"      

//    let charEval c : SM<char> = failwith "Not implemented"      

//    let boolEval b : SM<bool> = failwith "Not implemented"


//    type stm =                (* statements *)
//    | Declare of string       (* variable declaration *)
//    | Ass of string * aExp    (* variable assignment *)
//    | Skip                    (* nop *)
//    | Seq of stm * stm        (* sequential composition *)
//    | ITE of bExp * stm * stm (* if-then-else statement *)
//    | While of bExp * stm     (* while statement *)

//    let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

//(* Part 3 (Optional) *)

//    type StateBuilder() =

//        member this.Bind(f, x)    = f >>= x
//        member this.Return(x)     = ret x
//        member this.ReturnFrom(x) = x
//        member this.Delay(f)      = f ()
//        member this.Combine(a, b) = a >>= (fun _ -> b)
        
//    let prog = new StateBuilder()

//    let arithEval2 a = failwith "Not implemented"
//    let charEval2 c = failwith "Not implemented"
//    let rec boolEval2 b = failwith "Not implemented"

//    let stmntEval2 stm = failwith "Not implemented"

//(* Part 4 *) 

//    type word = (char * int) list
//    type squareFun = word -> int -> int -> Result<int, Error>

//    let stmntToSquareFun stm = failwith "Not implemented"


//    type coord = int * int

//    type boardFun = coord -> Result<squareFun option, Error> 

//    let stmntToBoardFun stm m = failwith "Not implemented"

//    type board = {
//        center        : coord
//        defaultSquare : squareFun
//        squares       : boardFun
//    }

//    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"