namespace FSharp.MachineLearning.Supervised.Experimental

module KNearestNeighbour =
    val classify  : float list -> float list list -> int -> 'a * int
