namespace FSharp.MachineLearning.Supervised.Experimental

module KNearestNeighbour =
    val classify : float list -> float list list -> 'a list -> int -> 'a * int when 'a : equality
    val minMaxNormalizer : float list list -> (float list -> float list)
    val normalize : 'a -> ('a -> 'b) -> 'b