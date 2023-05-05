// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    //type MultiSet<'a> = Temp of unit // Not implemented

    //let empty : MultiSet<'a> = Temp () // Not implemented
    //let add   : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a> = fun _ _ _ -> failwith "Not implemented" 
    //let fold  : ('b -> 'a -> uint32 -> 'b) -> 'b -> MultiSet<'a> -> 'b = fun _ _ _ -> failwith "Not implemented"

    type MultiSet<'a when 'a: comparison> = MS of Map<'a, uint32>
    let empty = MS Map.empty
    let isEmpty (MS s) = Map.isEmpty s
    let size (MS s) = s |> Map.fold (fun acc _ value -> acc + value) 0u
    let contains key (MS s) = Map.containsKey key s
    let numItems  key (MS s) = Map.tryFind key s |> Option.defaultValue 0u
    let add key times (MS s) = MS (s.Add(key, numItems key (MS s) + times))
    let addSingle key (MS s) = MS (s.Add(key, numItems key (MS s) + 1u))
    let remove key num (MS s) = match (numItems key (MS s)) with
                                                    | 0u -> MS s
                                                    | x when x <= num -> MS (s.Remove key)
                                                    | actualNum -> MS (s.Add (key, actualNum-num))
    let removeSingle key (MS s) = remove key 1u (MS s)
    let fold f acc (MS s) = Map.fold f acc s 
    let foldBack f (MS s) acc  = Map.foldBack f s acc 
    
