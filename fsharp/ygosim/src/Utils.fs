module Utils

module List =
    let setIndex index value list =
        List.mapi (fun previous i ->
            if i = index then value
            else previous)
