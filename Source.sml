(************************************************
 ** Source.sml
 ** sml
 ** 
 ** Aleksandar Nanevski
 ** 
 ** The abstract syntax for the intermediate
 ** language (which is a source to the compiler)
 ************************************************)


structure Source :> SOURCE =
struct
    exception Error of string

    datatype exp = 
	Plus of Var.var * Var.var
      | Minus of Var.var * Var.var
      | Times of Var.var * Var.var
      | Square of Var.var
      | UMinus of Var.var
	
    type assgmt = Var.var * exp

    datatype prog = Prog of {stages : (Var.var list * assgmt list) list,
			     symbTable : Var.dict, constTable : real VarTab.table}
	
	
    fun valToString (v, constTable) = 
	case VarTab.find(v, constTable) of
	    SOME r => Real.toString r
	  | NONE => Var.toString v

		
    fun expsToString (Plus (xs, ys), constTable) = 
	(valToString (xs, constTable))^" + "^(valToString (ys, constTable))
      | expsToString (Minus (xs, ys), constTable) = 
	(valToString (xs, constTable))^" - "^(valToString (ys, constTable))
      | expsToString (Times (xs, ys), constTable) = 
	(valToString (xs, constTable))^" * "^(valToString (ys, constTable))
      | expsToString (Square xs, constTable) = "sq "^(valToString (xs, constTable))
      | expsToString (UMinus xs, constTable) = "~ "^(valToString (xs, constTable))
	

    fun stageToString ((vs, a), constTable) =
	let fun varsToString nil = ""
	      | varsToString [v] = Var.toString v
	      | varsToString (v::vt) = (Var.toString v) ^ ", " ^ (varsToString vt)
	    val header = "[ " ^ varsToString vs ^ " ]"
	    fun string' nil = ""
	      | string' ((v, exp)::xt) =
		"    val "^(Var.toString v)^" = "^(expsToString (exp, constTable))^"\n"^(string' xt)
	in
	    "fn " ^ header ^ " => \n" ^
	    "let\n"^(string' a)^"end\n"
	end


    fun toString (Prog {stages=stages, symbTable=symbTable, constTable=constTable}) = 
	let fun printStages nil = ""
	      | printStages (s::st) =(stageToString (s, constTable)) ^ "\n" ^ (printStages st)
	in
	    printStages stages
	end

end