module Utils

module List =
    let setIndex index value =
        List.mapi (fun i previous ->
            if i = index then value
            else previous)


    let (.->) = setIndex

module Lens =
    open FSharpPlus.Lens

    let inline indexToLens index f (array: list<'a>) = f array.[index] <&> fun v -> List.setIndex index v array
