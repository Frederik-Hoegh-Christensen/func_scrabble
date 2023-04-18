// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.

    //The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    //for successful parses although running times and error messages will differ. Please report any inconsistencies.
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "True"
    let pFalse      = pstring "False"
    let pIsDigit    = pstring "IsDigit"
    let pIsLetter   = pstring "IsLetter"
    let pIsVowel   = pstring "IsVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "spaces"
    let spaces1        = many1 whitespaceChar <?> "space1" 

    let (.>*>.) p1 p2  = p1 .>> spaces .>>. p2 
    let (.>*>)  p1 p2  = p1 .>> spaces .>>  p2
    let (>*>.)  p1 p2  = p1 .>> spaces  >>. p2

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')'// incorrect (not implemented)

    let pid = (pletter <|> pchar '_')  .>>. many  (palphanumeric <|> pchar '_') |>> fun (a,b) ->  if b.IsEmpty then string a 
                                                                                                   else sprintf "%c%s" a (System.String(b|>Seq.toArray))

    
    let unop op p1 = op >*>. p1 

    let binop op p1 p2 = p1 .>*> op .>*>. p2  // incorrect (not implemented)

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CtomParse, cref = createParserForwardedToRef<cExp>() 

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse|>> Mul <?> "Mul"
    let DivParse = binop(pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop(pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    let NegParse = unop (pchar '-')  AtomParse  |>> (fun a  -> (N -1, a)) |>> Mul  <?>  "Neg"
    let VParse = pid |>> V <?> "Variable" 
    let PVParse = unop pPointValue AtomParse |>> PV <?> "pointValue"
    let CTIParse = unop pCharToInt CtomParse |>> CharToInt <?> "charToInt" 
    do aref := choice [CTIParse; NegParse; NParse; PVParse; VParse; ParParse]

    let AexpParse = TermParse

    


    let ParaParse = parenthesise CtomParse 
    let CParse = pchar '\'' >>. anyChar  .>> pchar '\''|>> C <?> "C"
    let CVParse = unop pCharValue AtomParse |>> CV <?> "charValue"
    let ToUpperParse = unop pToUpper CtomParse |>> ToUpper <?> "toUpper" 
    let ToLowerParse = unop pToLower CtomParse |>> ToLower <?> "toLower"
    let IntToCharParse = unop pIntToChar AtomParse |>> IntToChar <?> "intToChar"

    do cref := choice [ToLowerParse; ToUpperParse; IntToCharParse; CParse; CVParse; ParaParse]


    let CexpParse = CtomParse 

    let BexpParse = pstring "not implemented"

    let stmntParse = pstring "not implemented"



    let parseSquareProg _ = failwith "not implemented"

    let parseBoardProg _ = failwith "not implemented"
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}

    //let pIntToChar  = pstring "not implemented"
    //let pPointValue = pstring "not implemented"

    //let pCharToInt  = pstring "not implemented"
    //let pToUpper    = pstring "not implemented"
    //let pToLower    = pstring "not implemented"
    //let pCharValue  = pstring "not implemented"

    //let pTrue       = pstring "not implemented"
    //let pFalse      = pstring "not implemented"
    //let pIsDigit    = pstring "not implemented"
    //let pIsLetter   = pstring "not implemented"

    //let pif       = pstring "not implemented"
    //let pthen     = pstring "not implemented"
    //let pelse     = pstring "not implemented"
    //let pwhile    = pstring "not implemented"
    //let pdo       = pstring "not implemented"
    //let pdeclare  = pstring "not implemented"

    //let whitespaceChar = pstring "not implemented"
    //let pletter        = pstring "not implemented"
    //let palphanumeric  = pstring "not implemented"

    //let spaces         = pstring "not implemented"
    //let spaces1        = pstring "not implemented"

    //let (.>*>.) _ _ = failwith "not implemented"
    //let (.>*>) _ _  = failwith "not implemented"
    //let (>*>.) _ _  = failwith "not implemented"

    //let parenthesise _ = failwith "not implemented"

    //let pid = pstring "not implemented"

    
    //let unop _  = failwith "not implemented"
    //let binop _  = failwith "not implemented"

    //let TermParse, tref = createParserForwardedToRef<aExp>()
    //let ProdParse, pref = createParserForwardedToRef<aExp>()
    //let AtomParse, aref = createParserForwardedToRef<aExp>()

    //let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    //do tref := choice [AddParse; ProdParse]

    //let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    //do pref := choice [MulParse; AtomParse]

    //let NParse   = pint32 |>> N <?> "Int"
    //let ParParse = parenthesise TermParse
    //do aref := choice [NParse; ParParse]

    //let AexpParse = TermParse 

    //let CexpParse = pstring "not implemented"

    //let BexpParse = pstring "not implemented"

    //let stmParse = pstring "not implemented"

    //(* The rest of your parser goes here *)
    
    
    //// Default (unusable) board in case you are not implementing a parser for the DSL.
