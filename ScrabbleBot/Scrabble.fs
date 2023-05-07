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
    open MultiSet
    open System.Text
    let toSet (MS s) =
        s 
        |> Map.fold (fun acc (_,b) v -> 
            let set = Set.ofSeq(Seq.replicate (int v) b)
            Set.union acc set)
            Set.empty


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
            //let char = Map.minKeyValue charSet |> fst
            let charList = Map.toList charSet
            let charPair = charList.Head
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
                                printfn "Not a word!: %s\n%A" prefix charSet
                                stepper d (Map.remove char charSet) wordList builder count
                                 
            | None -> printfn "Char: %A" char
                      printfn "Wordlist: %A" wordList
                      
                      let builder = wordBuilder.Remove(wordBuilder.Length-1 , 1)
                      let updatedCharSet = charSet.Remove char |> Map.toList
                      let ucs = List.append updatedCharSet [charPair] |> Map.ofList
                      printfn "if char in tail? :%A" ucs
                      stepper dict ucs wordList builder count+1
                      
                      
                      
        for i in 0..hand.Count do
        stepper realDict hand List.empty (StringBuilder()) i


module Scrabble =
    open System.Threading


    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            //if st.playerNumber = 1u then printfn "player1moves: %A\n" st.hand
            //if st.playerNumber = 2u then printfn "player2moves: %A" st.hand
            let myFunc = (myMod.myFunction st pieces) |> Option.defaultValue 
            printfn "mybool: %A" myFunc 

            Print.printHand pieces (State.hand st)
            
            
            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            //printfn "board: %A" st.boardS
            
            let input =  System.Console.ReadLine()
            let move = RegEx.parseMove input
            

            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

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
        