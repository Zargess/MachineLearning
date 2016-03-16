namespace FSharp.MachineLearning.Supervised.Experimental

module KNearestNeighbour =
    val classify<'a when 'a : equality>  : float list -> float list list -> 'a list -> int -> 'a * int
