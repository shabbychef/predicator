(****************************************
 ** COMPILE.sml
 ** sml
 ** 
 ** Aleksandar Nanevski
 **
 ** Compiler for arithmetic expressions 
 ** TODO: add functions to write string
 ** directly to files
 ****************************************)


signature COMPILE =
sig
    structure S : SOURCE
    structure T : TARGET
    
    val parse : string -> S.prog
    val compile : S.prog -> T.prog

    val generate : string -> string
    val generateC : {name : string, prog : string} -> string

    (* compile prog and install in runtime-environment under name *)
    val install : {name : string, prog : string} -> unit
end