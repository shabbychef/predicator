signature NUMBER =
sig
    type t
    
    val zero : t
    val one : t
	
    val + : t * t -> t
    val - : t * t -> t
    val ~ : t -> t
    val * : t * t -> t
    val / : t * t -> t
	
    val == : t * t -> bool
    val < : t * t -> bool
    val > : t * t -> bool
    val <= : t * t -> bool
    val >= : t * t -> bool
    val compare : t * t -> order
    val min : t * t -> t
    val max : t * t -> t
    val sign : t -> int
    
    val toString : t -> string
end