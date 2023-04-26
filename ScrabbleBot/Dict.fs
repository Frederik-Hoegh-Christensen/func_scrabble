//module Dictionary
//    type Dict = Dicto of Set<string> 
//    let empty () = Dicto Set.empty
//    let insert s (Dicto d) = d.Add s|>Dicto
//    let lookup s (Dicto d) = d.Contains s 

module Dictionary
    
    type Dict = 
        | Leaf of bool
        | Node of bool * Map<char, Dict>
    let empty () = Leaf false

    let rec lookup (s: string) = function
    | Leaf b when s.Length = 0 -> b
    | Leaf _ -> false
    | Node (b, _) when s.Length = 0 -> b
    | Node (_, dict) ->
        match Map.tryFind s.[0] dict with
        | Some d -> lookup s.[1..] d
        | None -> false

    let rec insert (s:string) =
        function
        | Leaf _ when s.Length = 0 -> 
            Leaf true
        | Node (_, dict) when s.Length = 0 -> 
            Node(true, dict)

        | Leaf b ->
            let dict = Map.empty
            let updateDict = dict.Add(s.[0], insert s.[1..] (empty ()))
            Node (b, updateDict)

        | Node (b, dict) ->
            match Map.tryFind s.[0] dict with
            | Some d ->
                let updatedDict = dict.Add(s.[0], insert s.[1..] d)
                Node (b, updatedDict)
            | None ->
                let updatedDict = dict.Add(s.[0], insert s.[1..] (empty ()))
                Node (b, updatedDict)

    let rec step c =
        function 
        | Node (_, dict) -> 
            match dict.TryGetValue c with 
            | (true, value) ->
                match value with
                | Leaf b -> Some (b, value)
                | Node (b,_) -> Some (b,value)
            | (false, _) -> None
        | Leaf _ -> None
    
