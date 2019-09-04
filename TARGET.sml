(**********************************************
 ** TARGET.sml
 ** sml
 **
 ** Aleksandar Nanevski
 **
 ** Signature with abstract syntax and helper
 ** functions for the target language of the
 ** compiler
 **********************************************)


signature TARGET =
sig
    exception Error of string

    datatype num = 
	Var of Var.var | RealConst of string | Plus of num * num | Minus of num * num
      | Times of num * num | Square of num | Uminus of num | Oplus of num * num
      | Ominus of num * num | Otimes of num * num | Osquare of num | Tplus of num * num * num
      | Tminus of num * num * num | Ttimes of num * num * num | Tsquare of num * num 
      | Abs of num | Double of num | Approx of num

    datatype assgmt = 
	Eq of Var.var * num | 
	Susp of Var.var * {comp : assgmt list, lvar : Var.var list, rvar : Var.var list}
      | Lforce of Var.var * Var.var list | Rforce of Var.var * Var.var list

    datatype sign =
	Sign of num | SignTest of {num : num, err : num, comp : assgmt list, sign : sign}
	
    datatype stage = Stage of Var.var list * assgmt list

    datatype prog = Prog of {comp : stage list * sign, errs : (Var.var * Ulps.rat) list,
			     symbTable : Var.dict}


    (* fv (code, A) = (A - boundvars code) + freevars code *)
    val fv : assgmt list * VarSet.set -> VarSet.set

    val toString : prog -> string
    val toCString : {name : string, prog : prog} -> string
end