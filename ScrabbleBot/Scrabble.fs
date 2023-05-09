namespace YourClientName

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.


module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        boardS        : Map<coord, char * int>
    }

    let mkState b d pn h bs = {board = b; dict = d; playerNumber = pn; hand = h; boardS = bs; }

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    
    


module myMod = 

    
    let charToAlphabetNum (c: char) : uint32 =
        match c with
        | 'a' | 'A' -> uint32 1
        | 'b' | 'B' -> uint32 2
        | 'c' | 'C' -> uint32 3
        | 'd' | 'D' -> uint32 4
        | 'e' | 'E' -> uint32 5
        | 'f' | 'F' -> uint32 6
        | 'g' | 'G' -> uint32 7
        | 'h' | 'H' -> uint32 8
        | 'i' | 'I' -> uint32 9
        | 'j' | 'J' -> uint32 10
        | 'k' | 'K' -> uint32 11
        | 'l' | 'L' -> uint32 12
        | 'm' | 'M' -> uint32 13
        | 'n' | 'N' -> uint32 14
        | 'o' | 'O' -> uint32 15
        | 'p' | 'P' -> uint32 16
        | 'q' | 'Q' -> uint32 17
        | 'r' | 'R' -> uint32 18
        | 's' | 'S' -> uint32 19
        | 't' | 'T' -> uint32 20
        | 'u' | 'U' -> uint32 21
        | 'v' | 'V' -> uint32 22
        | 'w' | 'W' -> uint32 23
        | 'x' | 'X' -> uint32 24
        | 'y' | 'Y' -> uint32 25
        | 'z' | 'Z' -> uint32 26
        | _ -> failwith "Invalid character"
    type direction =
        |Right
        |Down
        |No

    let makeCoordList (startCoord: (int * int)) (direction: direction) (wordLength: int) =
        
        let (startX, startY) = startCoord
        let increment = if (direction = Down) then 1 else 0
        let xIncrement = if direction = Right then 1 else 0
        [for i in 0..wordLength-1 -> (startX + i*xIncrement, startY + i*increment)]

        

    let getPossibleMoves (st:State.state) (wordLength:int) (startCoord:(int*int)) =
        let board = st.boardS 

        let initRight = makeCoordList startCoord Right wordLength |> List.removeAt 0
        let (fstX,fstY) = initRight.Item 0
        let prevRightCoord = (fstX-2, fstY)
        let upperAdjacentRowStartCoord = (fstX, fstY+1)
        let lowerAdjacentRowStartCoord = (fstX, fstY-1)
        let (lastWordCoordX, lastWordCoordY) = initRight.Item (initRight.Length-1)
        let endAdjacentCoordRight = [(lastWordCoordX+1, lastWordCoordY)]
        let upperAdjacentRow = makeCoordList upperAdjacentRowStartCoord Right (wordLength-1)
        let lowerAdjacentRow = makeCoordList lowerAdjacentRowStartCoord Right (wordLength-1)
        let listRight = [prevRightCoord] @ initRight @ upperAdjacentRow @ lowerAdjacentRow @ endAdjacentCoordRight



        let initDown = makeCoordList startCoord Down wordLength |> List.removeAt 0 
        let (fstX,fstY) = initDown.Item 0
        let prevDownCoord = (fstX, fstY-2)
        let leftAdjacentRowStartCoord = (fstX-1, fstY)
        let rightAdjacentRowStartCoord = (fstX+1, fstY)
        let (lastWordCoordX, lastWordCoordY) = initRight.Item (initRight.Length-1)
        let endAdjacentCoordDown = [(lastWordCoordX, lastWordCoordY+1)]
        let leftAdjacentRow = makeCoordList leftAdjacentRowStartCoord Down (wordLength-1)
        let rightAdjacentRow = makeCoordList rightAdjacentRowStartCoord Down (wordLength-1)
        let listDown = [prevDownCoord] @ initDown @ leftAdjacentRow @ rightAdjacentRow @ endAdjacentCoordDown
        let hasKeyDown = List.exists (fun coord -> board.ContainsKey coord) listDown
        let hasKeyRight = List.exists (fun coord -> board.ContainsKey coord) listRight

        match hasKeyDown, hasKeyRight with 
        |true,true -> (No, List.empty)
        | true, false -> (Right, listRight)
        | false, true -> (Down, listDown)
        | false, false -> (Down, List.concat [listDown; listRight])


        //for coord in listRight do
        //    if board.Keys.Contains coord then
        //        for coor in listDown do
        //            if board.Keys.Contains coor then
        //                []
        //            else
        //                []
        //    else
        //        listDown


        //for coord in listRight do 
        //    if board.ContainsKey coord then 
        //        listDown 
        //    else 
                
        //for coord in listDown do 
        //    if board.ContainsKey coord then 

        //        listRight 
        //    else 
        //        List.empty 
    

    let makeMoveFromStrings (pieces: Map<uint32, tile>) (word:string) (startingCoord: coord) (direction: direction) =
        
        let coordList = makeCoordList startingCoord direction word.Length
        let matchingTiles = 
            [ for c in word do
                match pieces |> Map.tryFind (charToAlphabetNum c) with
                | Some tile -> yield charToAlphabetNum c, Set.minElement tile
                | None -> () ]

        
        let combinedList =
            List.zip coordList matchingTiles|> List.map (fun (coord, tile) -> (coord, tile))
        combinedList
    

        

            

    open MultiSet
    open System.Text
    open System
    let stepAll (s:string) (dict:Dictionary.Dict) =
        let mutable newDict = dict
        for c in s do
            newDict <- Dictionary.step c newDict |> Option.get |> snd
        newDict
    
    let myMove (st:State.state) (pieces: Map<uint32, tile>) = 
        let board = st.boardS
        let realDict = st.dict
        let hand =
            pieces
            |> Map.fold
                (fun acc k (v) ->
                    if MultiSet.contains k st.hand then
                        match v with
                        |  tile -> 
                            let charVal = tile |> Set.minElement |> fst
                            let pointVal = tile |> Set.minElement |> snd
                            Map.add charVal pointVal acc
                        
                    else
                        acc)
                Map.empty
        
        let rec stepper1 dict (boardChar: char) (hand:Map<char,int>) (builder:StringBuilder) (wordList:string list) =
            if hand.IsEmpty then
                        wordList
            else
            match Dictionary.step boardChar dict with

            | None ->
                
                
                

                let word = builder.ToString()
                let newDict = stepAll word realDict
                let charToBeRemoved = hand |> Map.maxKeyValue |> fst
                
                
                stepper1 newDict charToBeRemoved (hand.Remove charToBeRemoved) builder wordList

            | Some (true, d) ->
                let newBuilder = builder.Append(boardChar)
                let word = builder.ToString()
                let updatedWordList = word :: wordList
                let charToBeRemoved = (hand |> Map.minKeyValue |> fst)
                printfn "wordListfrom stepper1 : %A" updatedWordList
                stepper1 d charToBeRemoved (hand.Remove charToBeRemoved) newBuilder updatedWordList

            | Some (false, d) ->
                let newBuilder = builder.Append(boardChar)
                let charToBeRemoved = (hand |> Map.minKeyValue |> fst)
                stepper1 d charToBeRemoved (hand.Remove charToBeRemoved ) newBuilder wordList
        
        let join (p:Map<'a,'b>) (q:Map<'a,'b>) = 
            Map(Seq.concat [ (Map.toSeq p) ; (Map.toSeq q) ])
        let mutable finalWordList = []
        let mutable stringToCoordMap = Map.empty
        let boardList = board |> Map.toList
        for i in 0..boardList.Length-1 do
            let c = boardList.Item i |> snd |> fst
            let coord = boardList.Item i |> fst
            let aList = stepper1 realDict c hand (StringBuilder()) List.empty 
            let aMap =
                Map.ofList [
                for s in aList -> s, (coord)
                ]
            stringToCoordMap <- join stringToCoordMap aMap
            finalWordList <- finalWordList @ aList
        let possibleWords = finalWordList
                            |> List.map (fun word -> (word, getPossibleMoves st word.Length (Map.find word stringToCoordMap)))
                            |> List.map (fun word  -> (fst word, (snd word |> fst) ))
                            |> List.filter (fun (_, d) -> d <> No)
        let (fstString, dir) =  possibleWords |> List.head
        let stringCoord = Map.tryFind fstString stringToCoordMap
        printfn "mAXword: %A" fstString
        let acString = fstString.Remove(0,1)
        let (x,y) = stringCoord |> Option.get
        let xx = x+1
        let asger = (if dir = Down then (x, y+1) else (x+1, y))

        makeMoveFromStrings pieces (acString) asger dir
                   
        
        
        //stepper1 realDict 'A' hand (StringBuilder()) List.empty
        //stepper1 realDict 'A' (hand.Remove (hand |> Map.minKeyValue |> fst)) (StringBuilder()) List.empty

                //printfn "Char: %A" char
                //      printfn "Wordlist: %A" wordList
                //      if wordBuilder.Length = 0 then 
                //        wordList
                //      else
                //        let builder = wordBuilder.Remove(wordBuilder.Length-1 , 1)
                //        let updatedCharSet = charSet.Remove char |> Map.toList
                //        let ucs = List.append updatedCharSet [charPair] |> Map.ofList
                        
                //        stepper dict ucs wordList builder count
                //let builder = wordBuilder.Append(char)
                //                let word = wordBuilder.ToString()
                //                let updatedWordList = word :: wordList
                //                printfn "its a word!: %s\n%A" word charSet
                //                stepper d (Map.remove char charSet) updatedWordList builder count

                                
                                
            //| Some (false, d) -> 
            //                    let builder = wordBuilder.Append(char)
            //                    let prefix = wordBuilder.ToString()
            //                    //printfn "Not a word!: %s\n%A" prefix charSet
            //                    stepper d (Map.remove char charSet) wordList builder count
                                 
            //| None -> printfn "Char: %A" char
            //          printfn "Wordlist: %A" wordList



    let myFunction<'a> (st: State.state) (pieces: Map<uint32, tile>) =
        let IsFirstMove = st.boardS.IsEmpty
        let board = st.boardS
        let handIds = st.hand
        
        let hand =
            pieces
            |> Map.fold
                (fun acc k (v) ->
                    if MultiSet.contains k handIds then
                        match v with
                        |  tile -> 
                            let charVal = tile |> Set.minElement |> fst
                            let pointVal = tile |> Set.minElement |> snd
                            Map.add charVal pointVal acc
                        
                    else
                        acc)
                Map.empty

        let realDict = st.dict
        
        
        let rec stepper dict (charSet:Map<char,int>) (wordList) (wordBuilder: StringBuilder) count  = 
            if count = charSet.Count then
                wordList
            else
            //let char = Map.minKeyValue charSet |> fst
            let charList = Map.toList charSet
            let charPair = charList.Item count
            let char = charList.Item count |> fst
            

            
            match Dictionary.step (char) dict with

            
            | Some (true, d) -> 
                                let builder = wordBuilder.Append(char)
                                let word = wordBuilder.ToString()
                                let updatedWordList = word :: wordList
                                printfn "its a word!: %s\n%A" word charSet
                                stepper d (Map.remove char charSet) updatedWordList builder count
                                
            | Some (false, d) -> 
                                let builder = wordBuilder.Append(char)
                                let prefix = wordBuilder.ToString()
                                //printfn "Not a word!: %s\n%A" prefix charSet
                                stepper d (Map.remove char charSet) wordList builder count
                                 
            | None -> printfn "Char: %A" char
                      printfn "Wordlist: %A" wordList
                      if wordBuilder.Length = 0 then 
                        wordList
                      else
                        let builder = wordBuilder.Remove(wordBuilder.Length-1 , 1)
                        let updatedCharSet = charSet.Remove char |> Map.toList
                        let ucs = List.append updatedCharSet [charPair] |> Map.ofList
                        
                        stepper dict ucs wordList builder count
                      
                
        let mutable finalWordList = []
        for i in 0..hand.Count do
            let aList = stepper realDict hand List.empty (StringBuilder()) i 
            finalWordList <- finalWordList @ aList
            
        printfn "finalWordList: %A" finalWordList
        let fstString =  finalWordList |> List.max
        makeMoveFromStrings pieces (fstString) (0,0) Right
        



module Scrabble =
    open System.Threading


    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            //if st.playerNumber = 1u then printfn "player1moves: %A\n" st.hand
            //if st.playerNumber = 2u then printfn "player2moves: %A" st.hand
            
            
            
                
            
            
            Print.printHand pieces (State.hand st)
            
            
            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            //printfn "board: %A" st.boardS
            
            let input =  System.Console.ReadLine()

            let move = RegEx.parseMove input
            
            
            
            

            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            if st.boardS.IsEmpty then
                let myFunc = (myMod.myFunction st pieces) 
                let myMove = myFunc
                send cstream (SMPlay myMove)
            else
                let j = myMod.myMove st pieces
                send cstream (SMPlay j)
            
            
                //send cstream (SMPlay myMod.myMove st pieces)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                

                let rm = 
                    ms
                    |> List.fold (fun acc (c, (ui, (c, i))) -> MultiSet.remove ui 1u acc) st.hand

                let addPiecesList = 
                    newPieces
                    |> List.fold (fun acc (x, _) -> MultiSet.add x 1u acc) rm
                printfn "before board: %A" st.boardS
                let updatedBoardState = List.fold(fun acc (coord,(_, (x,y))) -> Map.add coord (x,y) acc ) st.boardS ms

                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' = {st with hand = addPiecesList; boardS = updatedBoardState} // This state needs to be updated
                
                //printfn "multiset: rm: %A" rm
                
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let updatedBoardState = List.fold(fun acc (coord,(_, (x,y))) -> Map.add coord (x,y) acc ) st.boardS ms
                let st' = {st with boardS = updatedBoardState}// This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A\n Boards:%A Move: %A " err st.boardS move; aux st


        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet Map.empty)
        