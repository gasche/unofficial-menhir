(* This is an implementation of Patricia trees, following Chris Okasaki's paper at the 1998 ML Workshop in Baltimore.
   Both big-endian and little-endian trees are provided. Both sets and maps are implemented on top of Patricia
   trees. *)

module Little : GMap.S with type key = int

module Big : GMap.S with type key = int

