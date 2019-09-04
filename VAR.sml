(******************************************
 ** VAR.sml
 ** sml
 ** 
 ** Aleksandar Nanevski
 **
 ** Signature for the Table of variables 
 ******************************************)

signature VAR =
sig
    exception Error of string

    eqtype var
    type dict
    type t = var

    datatype bind_mode = FREE | BOUND

    val empty : dict

    val compare : var * var -> order

    val newFree : string * dict -> var * dict
    val newBound : string * dict -> var * dict
    val setBind : var * bind_mode * dict -> dict
    val getBind : var * dict -> bind_mode
    val name : var -> string

    val isFree : var * dict -> bool

    val toString : var -> string
end



