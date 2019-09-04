(************************************************
 ** SOURCE.sml
 ** sml
 ** 
 ** Aleksandar Nanevski
 ** 
 ** The abstract syntax for the intermediate
 ** language (which is a source to the compiler
 ************************************************)

signature SOURCE =
sig
    exception Error of string

    datatype exp = 
	Plus of Var.var * Var.var
      | Minus of Var.var * Var.var
      | Times of Var.var * Var.var
      | Square of Var.var
      | UMinus of Var.var

    type assgmt = Var.var * exp

    (* a stage in the program consists of a list of free variables and a list of assignments *)
    datatype prog = Prog of {stages : (Var.var list * assgmt list) list,
			     symbTable : Var.dict, constTable : real VarTab.table}

    val toString : prog -> string
end
