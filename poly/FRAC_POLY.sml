(*********************************
 ** FRAC_POLY.sml
 ** sml
 **
 ** Aleksandar Nanevski
 **
 ** Rational Functions
 *********************************)


signature FRAC_POLY =
sig
    structure R : NUMBER
    structure P : POLY 
    sharing P.R = R

    type frac
    type t = frac
	
    val zero : t
    val one : t
    val X : t

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
    val min : t * t -> t
    val max : t * t -> t
    val compare : t * t -> order

    val sign : t -> int

    val toFrac : t -> {num : P.t, denom : P.t}
    val fromFrac : {num : P.t, denom : P.t} -> t
    val fromNumber : R.t -> t
    val fromPoly : P.t -> t
    val toString : t -> string
end