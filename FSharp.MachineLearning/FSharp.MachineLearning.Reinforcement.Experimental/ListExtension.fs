module List
    /// <summary>
    /// Replaces the current value at the given index
    /// </summary>
    /// <param name="list"></param>
    /// <param name="value"></param>
    /// <param name="index"></param>
    /// <exception cref="Index out of bounds"> Fails if index is negative or greater than the length of the list </exception>
    let rec replaceAt list value index =
        match list with
        | [] when index = 0 -> [ value; ]
        | [] when index > 0 -> failwith "Index bigger than list size!"
        | car::cdr when index > 0 -> car :: replaceAt cdr value (index - 1)
        | car::cdr when index = 0 -> value :: cdr
        | _ -> failwith "One or more errors occurred!"

    /// <summary>
    /// Finds all the indicies where the function returns true
    /// </summary>
    /// <param name="func"></param>
    /// <param name="list"></param>
    /// <param name="indicies"></param>
    /// <param name="counter"></param>
    let rec findAllIndicies func list indicies counter =
        match list with
        | [] -> indicies
        | hd::tl ->
            match hd with
            | x when func hd -> findAllIndicies func tl (counter::indicies) (counter + 1)
            | _ -> findAllIndicies func tl indicies (counter + 1)


