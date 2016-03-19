namespace FSharp.MachineLearning.Supervised.Experimental

module KNearestNeighbour =
    val classify : float list -> ('a * float list) list -> int -> 'a * int when 'a : equality