namespace YourClientName

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

type direction =
        |Right
        |Down
        |No
        |Both

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
        wordsPlayed         : int
        lastPlayedCoord : (int*int)
    }

    let mkState b d pn h bs = {board = b; dict = d; playerNumber = pn; hand = h; boardS = bs; wordsPlayed = 0; lastPlayedCoord = (0,0) }

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    
    


module myMod =
    let first (x, y, z) = x
    let second (x, y, z) = y
    let third (x, y, z) = z
    let charToAlphabetNum (c: char) : uint32*(char*int)  =
        match c with
        |'e' -> (0u, ('E', 0))
        | 'a' | 'A' -> (1u ,('A', 1))
        | 'b' | 'B' -> (2u, ('B', 3))
        | 'c' | 'C' -> (3u, ('C', 3))
        | 'd' | 'D' -> (4u, ('D', 2))
        | 'E' -> (5u, ('E', 1))
        | 'f' | 'F' -> (6u, ('F', 4))
        | 'g' | 'G' -> (7u, ('G', 2))
        | 'h' | 'H' -> (8u, ('H', 4))
        | 'i' | 'I' -> (9u, ('I', 1))
        | 'j' | 'J' -> (10u, ('J', 8))
        | 'k' | 'K' -> (11u, ('K', 5))
        | 'l' | 'L' -> (12u, ('L', 1))
        | 'm' | 'M' -> (13u, ('M', 3))
        | 'n' | 'N' -> (14u, ('N', 1))
        | 'o' | 'O' -> (15u, ('O', 1))
        | 'p' | 'P' -> (16u, ('P', 3))
        | 'q' | 'Q' -> (17u, ('Q', 10))
        | 'r' | 'R' -> (18u, ('R', 1))
        | 's' | 'S' -> (19u, ('S', 1))
        | 't' | 'T' -> (20u, ('T', 1))
        | 'u' | 'U' -> (21u, ('U', 1))
        | 'v' | 'V' -> (22u, ('V', 4))
        | 'w' | 'W' -> (23u, ('W', 4))
        | 'x' | 'X' -> (24u, ('X', 8))
        | 'y' | 'Y' -> (25u, ('Y', 4))
        | 'z' | 'Z' -> (26u, ('Z', 10))
        | _ -> failwith "Invalid character"
    

    let makeCoordList (startCoord: (int * int)) (direction: direction) (wordLength: int) =
        
        let (startX, startY) = startCoord
        

        if direction = Down then
            [for i in 0..wordLength-1 -> (startX , startY + i)]
        elif direction = Right then
            [for i in 0..wordLength-1 -> (startX + i , startY)]
        else
            List.empty


    let makeMoveFromStrings (st:State.state) (word:string) (startingCoord: coord) (direction: direction) =
        
        let hand = st.hand
        let coordList = makeCoordList startingCoord direction word.Length
        let matchingTiles = 
            [ for c in word do
                match hand |> MultiSet.contains (charToAlphabetNum c |> fst) with
                | true -> 
                    yield charToAlphabetNum c 
                    
                
                | false -> () ] 


        let combinedList =
            List.zip coordList matchingTiles|> List.map (fun (coord, tile) -> (coord, tile))
        combinedList
    
    

    open System.Text
    let stepAll (s:string) (dict:Dictionary.Dict) =
        let mutable newDict = dict
        for c in s do
            newDict <- Dictionary.step c newDict |> Option.get |> snd
        newDict

    let myNewMove (st:State.state) (pieces: Map<uint32, tile>) (dir:direction) (coord: (int*int))= 
        let x = fst coord
        let y = snd coord 
        let acCoord = (if dir = Right then (x+1, y) else (x, y+1))
        let dictChar = (st.boardS |> Map.tryFind coord) 
        let ogDict = (if st.boardS.IsEmpty then st.dict else (Dictionary.step (st.boardS |> Map.find coord |> fst) st.dict |> Option.get |> snd))

        //let (_, ogDict) = Dictionary.step 'G' ogDict |> Option.get // for debug purpose
        let hand =
            pieces
            |> Map.fold
                (fun acc k (v) ->
                    if MultiSet.contains k st.hand then
                        match v with
                        |  tile -> 
                            let charVal = tile |> Set.minElement |> fst
                            let pointVal = tile |> Set.minElement |> snd
                            
                            if pointVal = 0 then
                                Map.add 'e' pointVal acc
                            else
                            Map.add charVal pointVal acc
                        
                    else
                        acc)
                Map.empty
        
        let ogHandList = Map.toList hand
        //let ogHandList = ['R', 1; 'O',1; 'T',1; 'M',1; 'I',1;]
        
        let convertList lst =
            lst
            |> List.mapi (fun i (c, n) -> (i, c))
        let ogHandListChars = convertList ogHandList
        let rec permuteListToStrings lst = 
            match lst with
            | [] -> [""]
            | [(i, x)] -> [string x]
            | _ -> 
                lst 
                |> List.mapi (fun i (j, x) -> (j, x))
                |> List.collect (fun (j, x) -> 
                    permuteListToStrings (List.filter (fun (k, y) -> j <> k) lst)
                    |> List.map (fun s -> string x + s)
                )
        let allPerms = permuteListToStrings ogHandListChars
        let rec stepWord dict (word: string) (wordSet: string Set) (builder: StringBuilder) =
            let w = word
            if word.Length = 0 then
                Some wordSet
            else
            let wordToChars = word.ToCharArray()
            let firstChar = wordToChars |> Array.head
            match Dictionary.step firstChar dict with
            | Some (false, d) -> 
                let updatedBuilder = builder.Append(firstChar)
                let remOfWord = new string (wordToChars |> Array.removeAt(0))
                stepWord d remOfWord wordSet updatedBuilder
                
            | Some (true, d) ->
                let updatedBuilder = builder.Append(firstChar)
                let acWord = updatedBuilder.ToString()
                let updatedWordSet = wordSet.Add(acWord)
                let remOfWord = new string (wordToChars |> Array.removeAt(0))
                stepWord d remOfWord updatedWordSet updatedBuilder


            | None -> 
                if wordSet.IsEmpty then
                    None
                else
                    Some wordSet


        let rec stepPerm dict (checkWords: string list) (wordSet: string Set) = 
            if checkWords.Length = 0 then
                let i = wordSet
                wordSet
            else
            let word = checkWords |> List.head
            let updatedCheckWords = checkWords |> List.removeAt(0)
            match stepWord dict word Set.empty (StringBuilder()) with 
            | Some (words) ->
                let updatedWordSet = words |> Set.union wordSet
                stepPerm ogDict updatedCheckWords updatedWordSet
            | None ->
                stepPerm ogDict updatedCheckWords wordSet
        let allStrings = (stepPerm ogDict allPerms Set.empty)
        if allStrings.IsEmpty then
            None
        else
        let fstString = allStrings |> Set.toList |> List.max
        let firstMove = makeMoveFromStrings st (fstString) coord dir
        let moveElse = makeMoveFromStrings st (fstString) acCoord dir
        if st.wordsPlayed = 0 then
            Some firstMove
        else
        Some moveElse




module Scrabble =
    open System.Threading


    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =

            
            Print.printHand pieces (State.hand st)
            
            let smth = myMod.myNewMove st pieces
            // remove the force print when you move on from manual input (or when you have learnt the format)
            let lastCoordinate = st.lastPlayedCoord
            
            
            let playInDir = ( if st.wordsPlayed % 2 = 0 then Right else Down)
            let mutable newLastCoord = (0,0)
          
            let (myMove) = (myMod.myNewMove st pieces playInDir lastCoordinate) 
            match myMove with
            | Some move -> 
                let newXCoord = fst (move.Item (move.Length-1)) |> fst
                let newYCoord = fst (move.Item (move.Length-1)) |> snd
                newLastCoord <- (newXCoord, newYCoord)
                send cstream (SMPlay move)
            | None -> 
                let handlist = MultiSet.toList st.hand 
                send cstream (SMChange handlist)
                                //let changeTiles = (pieces |> Map.toList) |> List.fold (fun acc (k, v) -> k :: acc ) []
            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) myMove) // keep the debug lines. They are useful.          

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) myMove) // keep the debug lines. They are useful.
            
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                
                let rm = 
                    ms
                    |> List.fold (fun acc (c, (ui, (c, i))) -> MultiSet.remove ui 1u acc) st.hand

                let addPiecesList = 
                    newPieces
                    |> List.fold (fun acc (x, y) -> MultiSet.add x y acc) rm
                let updatedBoardState = List.fold(fun acc (coord,(_, (x,y))) -> Map.add coord (x,y) acc ) st.boardS ms
                
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' = {st with hand = addPiecesList;boardS = updatedBoardState; wordsPlayed = st.wordsPlayed + 1; lastPlayedCoord = newLastCoord} // This state needs to be updated
                
                //printfn "multiset: rm: %A" rm
                
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let updatedBoardState = List.fold(fun acc (coord,(_, (x,y))) -> Map.add coord (x,y) acc ) st.boardS ms
                let st' = {st with boardS = updatedBoardState}// This state needs to be updated
                aux st'
            |RCM (CMChangeSuccess(newPieces)) ->
                let newHand = 
                    newPieces
                    |> List.fold (fun acc (x, y) -> MultiSet.add x y acc) MultiSet.empty

                let st' = { st with hand = newHand }
                aux st' 
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()

                        
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            |RGPE err ->
                    match err with 
                    | [GPENotEnoughPieces(a,b)] -> 
                     let diff = a-b 
                     let Newhand = MultiSet.toList st.hand |> List.removeManyAt 0 (int diff)
                     let newHandSet = MultiSet.listToMultiset Newhand              

                     let st' = {st with hand = newHandSet }
                     aux st' 
                     |_ -> printfn "Gameplay Error:\n%A\n Boards:%A Move: %A " err st.boardS myMove; aux st

            


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
        