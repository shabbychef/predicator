(**************************************
 ** POLY.sml
 ** sml
 **
 ** Quick, slow, dirty and incomplete
 ** implementation of polynomials 
 **
 ***************************************)

signature POLY =
sig
    structure R : NUMBER

    type poly
    type t = poly

    val zero : poly
    val one : poly

    val X : poly

    val + : poly * poly -> poly
    val - : poly * poly -> poly
    val * : poly * poly -> poly
    val ~ : poly -> poly
    val scale : R.t * poly -> poly
   
    val == : poly * poly -> bool
    val < : poly * poly -> bool
    val > : poly * poly -> bool
    val <= : poly * poly -> bool
    val >= : poly * poly -> bool

    val min : poly * poly -> poly
    val max : poly * poly -> poly
    val compare : poly * poly -> order

    val sign : poly -> int

    (* ascending order of exponents *)
    val toCoefExpList : poly -> {coef : R.t, exp : int} list
    val fromCoefExpList : {coef : R.t, exp : int} list -> poly
    val fromNumber : R.t -> poly
    val toString : poly -> string
end
