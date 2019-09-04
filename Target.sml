(*************************************************
 ** Target.sml
 ** sml
 ** 
 ** Aleksandar Nanevski
 **
 ** The abstract syntax and SML and C printers
 ** for the target language.
 **
 ** TODO: 
 ** - the C printer can be improved a lot
 **   some possibilities stated in comments
 **   below
 ************************************************)


structure Target : TARGET =
struct
    exception Error of string

    datatype num = 
	Var of Var.var | RealConst of string | Plus of num * num | Minus of num * num
      | Times of num * num | Square of num | Uminus of num | Oplus of num * num
      | Ominus of num * num | Otimes of num * num | Osquare of num | Tplus of num * num * num
      | Tminus of num * num * num | Ttimes of num * num * num | Tsquare of num * num 
      | Abs of num | Double of num | Approx of num

    datatype assgmt = 
	Eq of Var.var * num | Susp of Var.var * {comp : assgmt list, lvar : Var.var list, rvar : Var.var list}
      | Lforce of Var.var * Var.var list | Rforce of Var.var * Var.var list

    datatype sign =
	Sign of num | SignTest of {num : num, err : num, comp : assgmt list, sign : sign}
	
    datatype stage = Stage of Var.var list * assgmt list


    (* the stage list got to be nonempty *)
    datatype prog = Prog of {comp : stage list * sign, errs : (Var.var * Ulps.rat) list,
			     symbTable : Var.dict}




    (******************************
     ** SML printer 
     ******************************)

    fun varsToString nil = ""
      | varsToString [v] = Var.toString v
      | varsToString (v::vs) = (Var.toString v)^", "^(varsToString vs)

    datatype tp = FLOAT | QUAD | XFLOAT of int

    fun numToString (n, typeTable) =
       (case n of
	    Var v => 
		(Var.toString v, valOf (VarTab.find (v, typeTable)))
	  | RealConst s => (s, FLOAT)
	  | Plus (n1, n2) => 
		let val (s1, tp1) = numToString (n1, typeTable)
		    val (s2, tp2) = numToString (n2, typeTable)
		in
		    case (tp1, tp2) of
			(FLOAT, FLOAT) => ("Q.sum(" ^ s1 ^ ", " ^ s2 ^ ")", QUAD)
		      | (FLOAT, QUAD) => ("Q.add(" ^ s2 ^ ", " ^ s1 ^ ")", XFLOAT 3)
		      | (QUAD, FLOAT) => ("Q.add(" ^ s1 ^ ", " ^ s2 ^ ")", XFLOAT 3)
		      | (QUAD, QUAD) => ("Q.+(" ^ s1 ^ ", " ^ s2 ^ ")", XFLOAT 4)
		      | (FLOAT, XFLOAT i2) => ("X.add(" ^ s2 ^ ", " ^ s1 ^ ")", XFLOAT(i2+1))
		      | (XFLOAT i1, FLOAT) => ("X.add(" ^ s1 ^ ", " ^ s2 ^ ")", XFLOAT(i1+1))
		      | (QUAD, XFLOAT i2) => ("Q.add2(" ^ s2 ^ ", " ^ s1 ^ ")", XFLOAT(i2+2))
		      | (XFLOAT i1, QUAD) => ("Q.add2(" ^ s1 ^ ", " ^ s2 ^ ")", XFLOAT(i1+2))
		      | (XFLOAT i1, XFLOAT i2) => ("X.+(" ^ s1 ^ ", " ^ s2 ^ ")", XFLOAT(i1+i2))
		end
	  | Minus (n1, n2) =>
		let val (s1, tp1) = numToString (n1, typeTable)
		    val (s2, tp2) = numToString (n2, typeTable)
		in
		    case (tp1, tp2) of
			(FLOAT, FLOAT) => ("Q.diff(" ^ s1 ^ ", " ^ s2 ^ ")", QUAD)
		      | (FLOAT, QUAD) => ("Q.isub(" ^ s1 ^ ", " ^ s2 ^ ")", XFLOAT 3)
		      | (QUAD, FLOAT) => ("Q.sub(" ^ s1 ^ ", " ^ s2 ^ ")", XFLOAT 3)
		      | (QUAD, QUAD) => ("Q.-(" ^ s1 ^ ", " ^ s2 ^ ")", XFLOAT 4)
		      | (FLOAT, XFLOAT i2) => ("X.isub(" ^ s1 ^ ", " ^ s2 ^ ")", XFLOAT(i2+1))
		      | (XFLOAT i1, FLOAT) => ("X.sub(" ^ s1 ^ ", " ^ s2 ^ ")", XFLOAT(i1+1))
		      | (QUAD, XFLOAT i2) => ("Q.isub2(" ^ s1 ^ ", " ^ s2 ^ ")", XFLOAT(i2+2))
		      | (XFLOAT i1, QUAD) => ("Q.sub2(" ^ s1 ^ ", " ^ s2 ^ ")", XFLOAT(i1+2))
		      | (XFLOAT i1, XFLOAT i2) => ("X.-(" ^ s1 ^ ", " ^ s2 ^ ")", XFLOAT(i1+i2))
		end
	  | Times (n1, n2) =>
		let val (s1, tp1) = numToString (n1, typeTable)
		    val (s2, tp2) = numToString (n2, typeTable)
		in
		    case (tp1, tp2) of
			(FLOAT, FLOAT) => ("Q.prod(" ^ s1 ^ ", " ^ s2 ^ ")", QUAD)
		      | (FLOAT, QUAD) => ("Q.scale(" ^ s1 ^ ", " ^ s2 ^ ")", XFLOAT 4)
		      | (QUAD, FLOAT) => ("Q.scale(" ^ s2 ^ ", " ^ s1 ^ ")", XFLOAT 4)
		      | (QUAD, QUAD) => ("Q.*(" ^ s1 ^ ", " ^ s2 ^ ")", XFLOAT 8)
		      | (FLOAT, XFLOAT i2) => ("X.scale(" ^ s1 ^ ", " ^ s2 ^ ")", XFLOAT(2*i2))
		      | (XFLOAT i1, FLOAT) => ("X.scale(" ^ s2 ^ ", " ^ s1 ^ ")", XFLOAT(2*i1))
		      | (QUAD, XFLOAT i2) => ("Q.scale2(" ^ s1 ^ ", " ^ s2 ^ ")", XFLOAT(4*i2))
		      | (XFLOAT i1, QUAD) => ("Q.scale2(" ^ s2 ^ ", " ^ s1 ^ ")", XFLOAT(4*i1))
		      | (XFLOAT i1, XFLOAT i2) => ("X.*(" ^ s1 ^ ", " ^ s2 ^ ")", XFLOAT(2*i1*i2))
		end
	  | Square n1 =>
		let val (s1, tp1) = numToString (n1, typeTable)
		in
		    case (n1, tp1) of
			(_, FLOAT) => ("Q.sq(" ^ s1 ^ ")", QUAD)
		      | (_, QUAD) => ("Q.sq2(" ^ s1 ^ ")", XFLOAT 6)
		      | ((Var _ | RealConst _), XFLOAT i) => ("X.*(" ^ s1 ^ ", " ^ s1 ^ ")", 
							      XFLOAT(i*(i+1)))
		      | (_, XFLOAT i) => ("(fn x => X.*(x, x))(" ^ s1 ^ ")", XFLOAT(i*(i+1)))
		end
	  | Uminus n1 =>
		let val (s1, tp1) = numToString (n1, typeTable)
		in
		    case tp1 of
			FLOAT => ("R.~(" ^ s1 ^ ")", FLOAT)
		      | QUAD => ("Q.~(" ^ s1 ^ ")", QUAD)
		      | XFLOAT i => ("X.~(" ^ s1 ^ ")", XFLOAT i)
		end
	  | Oplus (n1, n2) =>
		let val (s1, FLOAT) = numToString (n1, typeTable)
		    val (s2, FLOAT) = numToString (n2, typeTable)
		in
		    ("R.+(" ^ s1 ^ ", " ^ s2 ^ ")", FLOAT)
		end
	  | Ominus (n1, n2) =>
		let val (s1, FLOAT) = numToString (n1, typeTable)
		    val (s2, FLOAT) = numToString (n2, typeTable)
		in
		    ("R.-(" ^ s1 ^ ", " ^ s2 ^ ")", FLOAT)
		end
	  | Otimes (n1, n2) =>
		let val (s1, FLOAT) = numToString (n1, typeTable)
		    val (s2, FLOAT) = numToString (n2, typeTable)
		in
		    ("R.*(" ^ s1 ^ ", " ^ s2 ^ ")", FLOAT)
		end
	  | Osquare n1 =>
		let val (s1, FLOAT) = numToString (n1, typeTable)
		in
		    case n1 of
			(Var _ | RealConst _) => ("R.*(" ^ s1 ^ ", " ^ s1 ^ ")", FLOAT)
		      | _ => ("(fn x => R.*(x, x))(" ^ s1 ^ ")", FLOAT)
		end
	  | Tplus (n1, n2, n3) =>
		let val (s1, FLOAT) = numToString (n1, typeTable)
		    val (s2, FLOAT) = numToString (n2, typeTable)
		    val (s3, FLOAT) = numToString (n3, typeTable)
		in
		    ("#err (Q.toResErr(Q.sum(" ^ s1 ^ ", " ^ s2 ^ ")))", FLOAT)
		end
	  | Tminus (n1, n2, n3) =>
		let val (s1, FLOAT) = numToString (n1, typeTable)
		    val (s2, FLOAT) = numToString (n2, typeTable)
		    val (s3, FLOAT) = numToString (n3, typeTable)
		in
		    ("#err (Q.toResErr(Q.diff(" ^ s1 ^ ", " ^ s2 ^ ")))", FLOAT)
		end
	  | Ttimes (n1, n2, n3) => 
      		let val (s1, FLOAT) = numToString (n1, typeTable)
		    val (s2, FLOAT) = numToString (n2, typeTable)
		    val (s3, FLOAT) = numToString (n3, typeTable)
		in
		    ("#err (Q.toResErr(Q.prod(" ^ s1 ^ ", " ^ s2 ^ ")))", FLOAT)
		end
	  | Tsquare (n1, n2) =>
		let val (s1, FLOAT) = numToString (n1, typeTable)
		    val (s2, FLOAT) = numToString (n2, typeTable)
		in
		    ("#err (Q.toResErr(Q.sq(" ^ s1 ^ ", " ^ s2 ^ ")))", FLOAT)
		end
	  | Abs  n1 =>
		let val (s1, tp1) = numToString (n1, typeTable)
		in
		    case tp1 of
			FLOAT => ("R.abs(" ^ s1 ^ ")", FLOAT)
		      | QUAD => ("Q.abs(" ^ s1 ^ ")", QUAD)
		      | XFLOAT i => ("X.abs(" ^ s1 ^ ")", XFLOAT i)
		end
	  | Double n1 => 
		let val (s1, tp1) = numToString (n1, typeTable)
		in
		    case tp1 of
			FLOAT => ("R.*(2.0, " ^ s1 ^ ")", FLOAT)
		      | QUAD => ("Q.double(" ^ s1 ^ ")", QUAD)
		      | XFLOAT i => ("X.double(" ^ s1 ^ ")", XFLOAT i)
		end
	  | Approx n1 => 
		let val (s1, tp1) = numToString (n1, typeTable)
		in
		    case tp1 of
			FLOAT => (s1, FLOAT)
		      | QUAD => ("Q.approx(" ^ s1 ^ ")", FLOAT)
		      | XFLOAT _ => ("X.approx(" ^ s1 ^ ")", FLOAT)
		end)



    datatype suspTp = EMPTY | LEFT | RIGHT | FULL

    fun assgmtToString (a, tables as (typeTable, suspTable)) =
	case a of
	    Eq (v, n1) =>
		let val (s1, t1) = numToString (n1, typeTable)
		in
		    ("val "^(Var.toString v)^" = "^s1, 
		     (VarTab.insert ((v, t1), typeTable), suspTable))
		end
	  | Susp (v, {comp, lvar, rvar}) =>
		let val (s1, (typeTable, suspTable)) = 
		    assgmtsToString (comp, (typeTable, suspTable))
		    val s2 = varsToString lvar
		    val s3 = varsToString rvar
		    val header =  
			"val "^(Var.toString v)^" = susp (fn () => \n"^
			"let "^s1^"\n"^
			"in\n"
		in
		    case (lvar, rvar) of
			(nil, nil) => ("", (typeTable, VarTab.insert ((v, EMPTY), suspTable)))
		      | (_, nil) => 
			    (header^"("^s2^")\n"^
			     "end)\n", (typeTable, VarTab.insert ((v, LEFT), suspTable)))
		      | (nil, _) => 
			    (header^"("^s3^")\n"^
			     "end)\n", (typeTable, VarTab.insert ((v, RIGHT), suspTable)))
		      | (_, _) => 
			    (header^"(("^s2^"), ("^s3^"))\n"^
			     "end)\n", (typeTable, VarTab.insert ((v, FULL), suspTable)))
		end
	  | Lforce (v, vars) =>
		let val s1 = varsToString vars
		in
		    case (valOf (VarTab.find(v, suspTable))) of
			EMPTY => ("", tables)
		      | LEFT => ("val ("^s1^") = "^(Var.toString v)^"()\n", tables)
		      | RIGHT => ("", tables)
		      | FULL => ("val (("^s1^"), _) = "^(Var.toString v)^"()\n", tables)
		end
	  | Rforce (v, vars) =>
		let val s1 = varsToString vars
		in
		    case VarTab.find (v, suspTable) of
			NONE => ("", tables) (* the initial suspensions *)
		      | SOME EMPTY => ("", tables)
		      | SOME LEFT => ("", tables)
		      | SOME RIGHT => ("val ("^s1^") = "^(Var.toString v)^"()\n", tables)
		      | SOME FULL => ("val (_, ("^s1^")) = "^(Var.toString v)^"()\n", tables)
		end
		

    and assgmtsToString (comp, tables) =
	(case comp of
	     nil => ("", tables)
	   | (a::t) =>
		 let val (s1, tables) = assgmtToString (a, tables)
		     val (s2, tables) = assgmtsToString (t, tables)
		 in
		     (s1^"\n"^
		      s2, tables)
		 end)
	    
		
	     
    fun signToString (sg, tables as (typeTable, suspTable)) =
	case sg of
	    Sign n1 => 
		let val (s1, tp1) = numToString (n1, typeTable)
		in
		    case tp1 of
			FLOAT => ("R.sign("^s1^")", tables)
		      | QUAD => ("Q.sign("^s1^")", tables)
		      | XFLOAT _ => ("X.sign("^s1^")", tables)
		end
	  | SignTest {num, err, comp, sign} => 
		let val (s1, FLOAT) = numToString (num, typeTable)
		    val (s2, FLOAT) = numToString (err, typeTable)
		    val (s3, tables) = assgmtsToString (comp, tables)
		    val (s4, tables) = signToString (sign, tables)
		in
		    ("if "^s1^" > "^s2^" then 1\n"^
		     "else if R.~("^s1^") > "^s2^" then ~1\n"^
		     "     else \n"^
		     "let "^s3^"\n"^
		     "in\n"^
		     s4^"\n"^
		     "end\n", tables)
		end


    fun stageToString (Stage(vars, a), (typeTable, suspTable)) =
	let val typeTable = 
	      foldl (fn (v, t) => VarTab.insert((v, FLOAT), t)) typeTable vars
	    val s1 = varsToString vars
	    val (s2, tables) = assgmtsToString (a, (typeTable, suspTable))
	in
	    ("fn [ "^s1^" ] =>\n"^
	     "let "^s2, tables)
	end


    fun stagesToString (nil, ends, tables) = ("", ends, tables)
      | stagesToString (s::ss, ends, tables) =
	let val (s1, tables) = stageToString (s, tables)
	    val (s2, ends, tables) = stagesToString (ss, ends, tables)
	in
	    (s1^"\n"^
             "in\n"^
             s2, 
	     "end\n"^
	     ends, tables)
	end

    fun errToString (v, q) = "val "^(Var.toString v)^" = "^(Ulps.toString q)

    fun errsToString nil = ""
      | errsToString (e::es) =
	(errToString e)^"\n"^
	(errsToString es)

    fun toString (Prog {comp, errs, symbTable}) =
	let val (stages, sign) = comp
	    val s0 = errsToString errs
	    fun upd ((v, _), t) = VarTab.insert ((v, FLOAT), t)
	    val typeTable = foldl upd VarTab.empty errs
	    val (s1, ends, tables) = stagesToString (stages, "", (typeTable, VarTab.empty))
	    val (s2, tables) = signToString (sign, tables)
	in
	    "let val E = R.fromManExp{man=1.0, exp= ~53}\n"^
	    "    val rm = IEEEReal.getRoundingMode()\n"^
	    "    val _ = IEEEReal.setRoundingMode IEEEReal.TO_POSINF\n"^
	    s0^
	    "    val _ = IEEEReal.setRoundingMode rm\n"^
	    "in\n"^
	    s1^"\n"^
	    s2^"\n"^
	    ends ^
	    "end (* main let *)\n"
	end





    (******************************************
     ** C printer
     ** 
     ** TODO: 
     ** - memory managements i.e. reuse of 
     **   arrays allocated for temporary values
     ** - compression of large arrays
     ** - better routines for multiplication
     **   and squaring
     ******************************************)

    (* Invariant : add only variables starting with t to *)
    (* the symbol tables involved. The scheme relies on properties *)
    (* of Var.newBound to produce var-names with given prefix *)
    (* Hacky but useful at the moment *)

    (* global variables in Shewchuk's library *)
    (* make sure never to contain a variable starting with t *)
    val reservedNames = 
	["c", "abig", "ahi", "alo", "bhi", "blo", 
	 "err1", "err2", "err3", "_i", "bvirt", "avirt", 
	 "bround", "around", "_j", "_0"]

    fun reserved s =
	let fun search nil = false
	      | search (name::names) = 
	        (s = name) orelse search names
	in
	    search reservedNames
	end

    fun varToCString (v, symbTable) =
	let val s = Var.toString v
	in
	    if reserved s then
		(* get a new name, different from all others *)
		(* renames consistently due to the invariant *)
		let val (y, _) = Var.newBound (s, symbTable)
		in
		    Var.toString y
		end
	    else s
	end


    fun strListToString nil = ""
      | strListToString [s] = s
      | strListToString (s::ss) = s ^ ", " ^ (strListToString ss)

    fun varsToCString (nil, symbTable) = ""
      | varsToCString ([v], symbTable) = varToCString (v, symbTable)
      | varsToCString (v::vs, symbTable) =
	(varToCString (v, symbTable))^", "^(varsToCString (vs, symbTable))


    (* return string that is a code of computations for the expression *)
    (* and a handle on the expression (a fresh variable if needed) *)

    fun numToCString (n, tables as (symbTable, typeTable)) =
	case n of 
	    Var v =>
		("", varToCString(v, symbTable), valOf (VarTab.find (v, typeTable)), tables)
	  | RealConst s => 
		let val c = valOf (Real.fromString s)
		in
		    if c > 0.0 then ("", s, FLOAT, tables)
		    else ("", "-"^(Real.toString (~ c)), FLOAT, tables)
		end
	  | _ =>
		let val (v, symbTable) = Var.newBound ("t", symbTable) (* invariant preserved *)
		    val (code, tp, (symbTable, typeTable)) = 
			assgmtToCString (Eq (v, n), (symbTable, typeTable))
		in
		    (code, varToCString(v, symbTable), tp, 
		     (symbTable, VarTab.insert ((v, tp), typeTable)))
		end




    (* return string that is a code of expression, type of it and tables *)

    and assgmtToCString (Eq (y, n), tables as (symbTable, typeTable)) =
	let val s = varToCString(y, symbTable)
	in
	    case n of
		Var v => 
		    let val s1 = varToCString(v, symbTable)
			val tp1 = valOf (VarTab.find (v, typeTable))
			val code = 
			    case tp1 of
				FLOAT => s ^ " = " ^ s1
			      | QUAD => 
				    s ^ "_1 = " ^ s1 ^ "_1;\n" ^ 
				    s ^ "_2 = " ^ s1 ^ "_2"
			      | XFLOAT _ => 
				    (* no need to copy since values don't mutate *)
				    s ^ " = " ^ s1 ^ ";\n" ^
				    s ^ "_len = " ^ s1 ^ "_len"
		    in
			(code^";\n", tp1, tables)
		    end
	  | RealConst c => 
		let val cc = valOf (Real.fromString c)
		in
		    if cc > 0.0 then (s^" = "^c^";\n", FLOAT, tables)
		    else (s^" = -"^(Real.toString (~ cc))^";\n", FLOAT, tables)
		end
	  | Plus (n1, n2) => 
		let val (code1, s1, tp1, tables) = numToCString (n1, tables)
		    val (code2, s2, tp2, tables) = numToCString (n2, tables)
		    val (code, tp) =
			case (tp1, tp2) of
			    (FLOAT, FLOAT) => 
				("Two_Sum(" ^ s1 ^ ", " ^ s2 ^ ", " ^ s ^ "[1], " ^ s ^ "[0])", QUAD)
			  | (FLOAT, QUAD) => 
				("Two_One_Sum(" ^ s2 ^ "[1], " ^ s2 ^ "[0], " ^ 
				 s1 ^ ", " ^ s ^ "[2], " ^ s ^ "[1], " ^ s ^ "[0]); \n" ^ s ^ "_len = 3",
				 XFLOAT 3)
			  | (QUAD, FLOAT) => 
				("Two_One_Sum(" ^ s1 ^ "[1], " ^ s1 ^ "[0], " ^ 
				 s2 ^ ", " ^ s ^ "[2], " ^ s ^ "[1], " ^ s ^ "[0]); \n" ^ s ^ "_len = 3", 
				 XFLOAT 3)
			  | (QUAD, QUAD) => 
				("Two_Two_Sum(" ^ s1 ^ "[1], " ^ s1 ^ "[0], " ^ 
				 s2 ^ "[1], " ^ s2 ^ "[0], " ^ s ^ "[3], " ^ s ^ "[2], " ^ 
				 s ^ "[1], " ^ s ^ "[0]); \n" ^ s ^ "_len = 4", 
				 XFLOAT 4)
			  | (FLOAT, XFLOAT i2) => 
				(s ^ "_len = grow_expansion_zeroelim(" ^ s2 ^ "_len," ^ s2 ^ ", " ^ s1 ^ ")",
				 XFLOAT(i2+1))
			  | (XFLOAT i1, FLOAT) => 
				(s ^ "_len = grow_expansion_zeroelim(" ^ s1 ^ "_len, " ^ s1 ^ ", " ^ s2 ^ ")", 
				 XFLOAT(i1+1))
			  | (QUAD, XFLOAT i2) => 
				(s ^ "_len = fast_expansion_sum_zeroelim(2, " ^ s1 ^ ", " ^ s2 ^ "_len, " ^ 
				 s2 ^ ", " ^ s ^ ")", 
				 XFLOAT(2+i2))
			  | (XFLOAT i1, QUAD) => 
				(s ^ "_len = fast_expansion_sum_zeroelim(2, " ^ s2 ^ ", " ^ s1 ^ "_len, " 
				 ^ s1 ^ ", " ^ s ^ ")", 
				 XFLOAT(i1+2))
			  | (XFLOAT i1, XFLOAT i2) => 
				(s ^ "_len = fast_expansion_sum_zeroelim(" ^ s2 ^ "_len, " ^ s2 ^ ", " 
				 ^ s1 ^ "_len, " ^ s1 ^ ", " ^ s ^ ")", 
				 XFLOAT(i1+i2))
		in
		    (code1^code2^code^";\n", tp, tables)
		end
	  | Minus (n1, n2) => 
		let val (code1, s1, tp1, tables) = numToCString (n1, tables)
		    val (code2, s2, tp2, tables) = numToCString (n2, tables)
		    val (code, tp) =
			case (tp1, tp2) of
			    (FLOAT, FLOAT) => 
				("Two_Diff(" ^ s1 ^ ", " ^ s2 ^ ", " ^ s ^ "[1], " ^ s ^ "[0])", 
				 QUAD)
			  | (FLOAT, QUAD) => 
				("One_Two_Diff(" ^ s1 ^ ", " ^ s2 ^ "[1], " ^ s2 ^ "[0], " ^ 
				 s ^ "[2], " ^ s ^ "[1], " ^ s ^ "[0])", 
				 XFLOAT 3)
			  | (QUAD, FLOAT) => 
				("Two_One_Diff(" ^ s1 ^ "[1], " ^ s1 ^ "[0], " ^ s2 ^ ", " ^ 
				 s ^ "[2], " ^ s ^ "[1], " ^ s ^ "[0])", 
				 XFLOAT 3)
			  | (QUAD, QUAD) => 
				("Two_Two_Diff(" ^ s1 ^ "[1], " ^ s1 ^ "[0], " ^ s2 ^ "[1], " ^ 
				 s2 ^ "[0], " ^ s ^ "[3], " ^ s ^ "[2], " ^ s ^ "[1], " ^ 
				 s ^ "[0]); \n" ^ s ^ "_len = 4", 
				 XFLOAT 4)
			  | (FLOAT, XFLOAT i2) => 
				(s ^ "_len = shrink_expansion(" ^ s1 ^ ", " ^ s2 ^ "_len, " ^ s2 ^ ", " ^ s ^ ")",
				 XFLOAT(1+i2))
			  | (XFLOAT i1, FLOAT) => 
				(s ^ "_len = grow_expansion(" ^ s1 ^ "_len, " ^ s1 ^ ", -" ^ s2 ^ ", " ^ s ^ ")",
				 XFLOAT(i1+1))
			  | (QUAD, XFLOAT i2) => 
				(s ^ "_len = fast_expansion_diff_zeroelim(2, " ^ s1 ^ ", " ^
				 s2 ^ "_len, " ^ s2 ^ ", " ^ s ^ ")", 
				 XFLOAT(2+i2))
			  | (XFLOAT i1 , QUAD) => 
				(s ^ "_len = fast_expansion_diff_zeroelim(" ^ s1 ^ "_len, " ^ s1 ^ ", " ^ 
				 "2, " ^ s2 ^ ", " ^ s ^ ")",
				 XFLOAT(i1+2))
			  | (XFLOAT i1, XFLOAT i2) => 
				(s ^ "_len = fast_expansion_diff_zeroelim(" ^ s1 ^ "_len, " ^ s1 ^ ", " 
				 ^ s2 ^ "_len, " ^ s2 ^ ", " ^ s ^ ")", 
				 XFLOAT(i1+i2))
		in
		    (code1^code2^code^";\n", tp, tables)
		end
	  | Times (n1, n2) =>
		let val (code1, s1, tp1, tables) = numToCString (n1, tables)
		    val (code2, s2, tp2, tables) = numToCString (n2, tables)
		    val (code, tp) =
			case (tp1, tp2) of		
			    (FLOAT, FLOAT) => 
				("Two_Product(" ^ s1 ^ ", " ^ s2 ^ ", " ^ s ^ "[1], " ^ s ^ "[0])",
				 QUAD)
			  | (FLOAT, QUAD) => 
				("Two_One_Product(" ^ s2 ^ "[1], " ^ s2 ^ "[0], " ^ 
				 s1 ^ ", " ^ s ^ "[3], " ^ s ^ "[2], " ^ s ^ "[1], " ^ s ^ "[0]); \n" ^
				 s ^ "_len = 4",
				 XFLOAT 4)
			  | (QUAD, FLOAT) => 
				("Two_One_Product(" ^ s1 ^ "[1], " ^ s1 ^ "[0], " ^ 
				 s2 ^ ", " ^ s ^ "[3], " ^ s ^ "[2], " ^ s ^ "[1], " ^ s ^ "[0]); \n" ^
				 s ^ "_len = 4", 
				 XFLOAT 4)
			  | (QUAD, QUAD) => 
				("Two_Two_Product(" ^ s1 ^ "[1], " ^ s1 ^ "[0], " ^ s2 ^ "[1], " ^ 
				 s2 ^ "[0], " ^ s ^ "[7], " ^ s ^ "[6], " ^ s ^ "[5], " ^ s ^ "[4], " ^
				 s ^ "[3], " ^ s ^ "[2], " ^ s ^ "[1], " ^ s ^ "[0]); \n" ^ 
				 s ^ "_len = 8",
				 XFLOAT 8)
			  | (FLOAT, XFLOAT i2) => 
				(s ^ "_len = scale_expansion_zeroelim(" ^ s2 ^ "_len, " ^ s2 ^ ", " ^ s1 ^ ", " ^ s ^ ")",
				 XFLOAT(2*i2))
			  | (XFLOAT i1, FLOAT) => 
				(s ^ "_len = scale_expansion_zeroelim(" ^ s1 ^ "_len, " ^ s1 ^ ", " ^ s2 ^ ", " ^ s ^ ")",
				 XFLOAT(2*i1))
			  | (QUAD, XFLOAT i2) => 
				(s ^ "_len = mult_expansion_zeroelim(" ^ s2 ^ "_len, " ^ s2 ^ ", 2, " ^ s1 ^ ", " ^ s ^ ")",
				 XFLOAT(4*i2))
			  | (XFLOAT i1, QUAD) => 
				(s ^ "_len = mult_expansion_zeroelim(" ^ s1 ^ "_len, " ^ s1 ^ ", 2, " ^ s2 ^ ", " ^ s ^ ")",
				 XFLOAT(4*i1))
			  | (XFLOAT i1, XFLOAT i2) => 
				(s ^ "_len = mult_expansion_zeroelim(" ^ s1 ^ "_len, " ^ s1 ^ ", " 
				 ^ s2 ^ "_len, " ^ s2 ^ ", " ^ s ^ ")",
				 XFLOAT(2*i1*i2)) 
		in
		    (code1^code2^code^";\n", tp, tables)
		end
	  | Square n1 =>
		let val (code1, s1, tp1, tables) = numToCString (n1, tables)
		    val (code, tp) =
			case tp1 of				
			    FLOAT => 
				("Square(" ^ s1 ^ ", " ^ s ^ "[1], " ^ s ^ "[0])", 
				 QUAD)
			  | QUAD => 
				("Two_Square(" ^ s1 ^ ", " ^ s ^ "[5], " ^ s ^ "[4], " ^ 
				s ^ "[3], " ^ s ^ "[2], " ^ s ^ "[1], " ^ s ^ "[0]);\n" ^ s ^ "_len = 6", 
				 XFLOAT 6)
			  | XFLOAT i => 
				(s ^ "_len = mult_expansion_zeroelim(" ^ s1 ^ "_len, " ^ s1 ^ ", " ^ 
				 s1 ^ "_len, " ^ s1 ^ ", " ^ s ^ ")",
				 XFLOAT(2*i*i)) 
		                (* can be made i*(i+1) with a better routine for squaring *)
		in
		    (code1^code^";\n", tp, tables)
		end
	  | Uminus n1 => 
		let val (code1, s1, tp1, tables) = numToCString (n1, tables)
		    val (code, tp) =
			case tp1 of
			    FLOAT => (s^" = -"^s1, FLOAT)
			  | QUAD => 
				("Two_UMinus(" ^ s1 ^ "[1], " ^ s1 ^ "[0], " ^ s ^ "[1], " ^ s ^ "[0])",
				 QUAD)
			  | XFLOAT i => 
				(s ^ "_len = expansion_uminus(" ^ s1 ^ "_len, " ^ s1 ^ ", " ^ s ^ ")",
				 XFLOAT i)
		in
		    (code1^code^";\n", tp, tables)
		end
	  | Oplus (n1, n2) =>
		let val (code1, s1, FLOAT, tables) = numToCString (n1, tables)
		    val (code2, s2, FLOAT, tables) = numToCString (n2, tables)
		    val code = s ^ " = (REAL)(" ^ s1 ^ " + " ^ s2 ^ ")"
		in
		    (code1^code2^code^";\n", FLOAT, tables)
		end
	  | Ominus (n1, n2) =>
		let val (code1, s1, FLOAT, tables) = numToCString (n1, tables)
		    val (code2, s2, FLOAT, tables) = numToCString (n2, tables)
		    val code = s ^ " = (REAL)(" ^ s1 ^ " - " ^ s2 ^ ")" 
		in
		    (code1^code2^code^";\n", FLOAT, tables)
		end
	  | Otimes (n1, n2) => 
		let val (code1, s1, FLOAT, tables) = numToCString (n1, tables)
		    val (code2, s2, FLOAT, tables) = numToCString (n2, tables)
		    val code = s ^ " = (REAL)(" ^ s1 ^ " * " ^ s2 ^ ")"
		in
		    (code1^code2^code^";\n", FLOAT, tables)
		end
	  | Osquare n1 =>
		let val (code1, s1, FLOAT, tables) = numToCString (n1, tables)
		    val code = s ^ " = (REAL)(" ^ s1 ^ " * " ^ s1 ^ ")"
		in
		    (code1^code^";\n", FLOAT, tables)
		end 
	  | Tplus (n1, n2, n3) =>
		let val (code1, s1, FLOAT, tables) = numToCString (n1, tables)
		    val (code2, s2, FLOAT, tables) = numToCString (n2, tables)
		    val (code3, s3, FLOAT, tables) = numToCString (n3, tables)
		    val code = "Two_Sum_Tail(" ^ s1 ^ ", " ^ s2 ^ ", " ^ s3 ^ ", " ^ s ^ ")"
		in
		    (code1^code2^code3^code^";\n", FLOAT, tables)
		end
	  | Tminus (n1, n2, n3) =>
		let val (code1, s1, FLOAT, tables) = numToCString (n1, tables)
		    val (code2, s2, FLOAT, tables) = numToCString (n2, tables)
		    val (code3, s3, FLOAT, tables) = numToCString (n3, tables)
		    val code = "Two_Diff_Tail(" ^ s1 ^ ", " ^ s2 ^ ", " ^ s3 ^ ", " ^ s ^ ")"
		in
		    (code1^code2^code3^code^";\n", FLOAT, tables)
		end
	  | Ttimes (n1, n2, n3) =>
		let val (code1, s1, FLOAT, tables) = numToCString (n1, tables)
		    val (code2, s2, FLOAT, tables) = numToCString (n2, tables)
		    val (code3, s3, FLOAT, tables) = numToCString (n3, tables)
		    val code = "Two_Product_Tail(" ^ s1 ^ ", " ^ s2 ^ ", " ^ s3 ^ ", " ^ s ^ ")"
		in
		    (code1^code2^code3^code^";\n", FLOAT, tables)
		end
	  | Tsquare (n1, n2) =>
		let val (code1, s1, FLOAT, tables) = numToCString (n1, tables)
		    val (code2, s2, FLOAT, tables) = numToCString (n2, tables)
		    val code = "Square_Tail(" ^ s1 ^ ", " ^ s2 ^ ", " ^ s ^ ")"
		in
		    (code1^code2^code^";\n", FLOAT, tables)
		end
	  | Abs n1 =>
		let val (code1, s1, tp1, tables) = numToCString (n1, tables)
		    val code = 
			case tp1 of 
			    FLOAT => s ^ " = Absolute(" ^ s1 ^ ")"
			  | QUAD => "Two_Absolute("^s1^"[1], "^s1^"[0], "^s^"[1], "^s^"[0])"
			  | XFLOAT _ => s^"_len = expansion_absolute("^s1^"_len, "^s1^", "^s^")"
		in
		    (code1^code^";\n", tp1, tables)
		end
	  | Double n1 =>
		let val (code1, s1, tp1, tables) = numToCString (n1, tables)
		    val code = 
			case tp1 of 
			    FLOAT => s^" = (REAL)(2.0 * "^s1^")"
			  | QUAD => s^"[1] = 2.0 * "^s1^"[1]; "^s^"[0] = 2.0 * "^s1^"[0]"
			  | XFLOAT _ => s^"_len = expansion_double("^s1^"_len, "^s1^", "^s^")"
		in
		    (code1^code^";\n", tp1, tables)
		end
	  | Approx n1 =>
		let val (code1, s1, tp1, tables) = numToCString (n1, tables)
		    val code = 
			case tp1 of 
			    FLOAT => s ^ " = " ^ s1 
			  | QUAD => s ^ " = " ^ s1 ^ "[1])"
			  | XFLOAT _ => s ^ " = estimate(" ^ s1 ^ "_len, " ^ s1 ^ ")"
		in
		    (code1^code^";\n", FLOAT, tables)
		end
	end
      | assgmtToCString (a, tables) =
	(case a of
	     (Susp (v, {comp=_,lvar=nil,rvar=nil}) |
	      Lforce (v, nil) | Rforce (v, nil)) => ("", FLOAT, tables)
	    | _ => raise Error "not yet implemented for C target language")


    fun assgmtsToCString (comp, tables) =
	case comp of
	    nil => ("", tables)
	  | ((a as Eq (y, _))::aa) =>
		let val (s1, tp, (symbTable, typeTable)) = assgmtToCString (a, tables)
		    val typeTable = VarTab.insert ((y, tp), typeTable)
		    val (s2, tables) = assgmtsToCString (aa, (symbTable, typeTable))
		in
		     (s1^s2, tables)
		 end
          | (a::aa) =>
	    (case a of
		 (Susp (v, {comp=_,lvar=nil,rvar=nil}) |
		  Lforce (v, nil) | Rforce (v, nil)) => assgmtsToCString (aa, tables)
		| _ => raise Error "not yet implemented for C target language")
		 




    fun signToCString (sg, tables as (typeTable, suspTable)) =
	case sg of
	    Sign n1 => 
		let val (code1, s1, tp1, tables) = numToCString (n1, tables)
		    val code =
			case tp1 of
			    FLOAT => 
				"if (" ^ s1 ^ " > 0.0) return 1; else\n"^
				"if (" ^ s1 ^ " < 0.0) return -1; else 0"
			  | QUAD => 
				"if (" ^ s1 ^ "[1] > 0.0) return 1; else\n"^
				"if (" ^ s1 ^ "[1] < 0.0) return -1; else 0"
			  | XFLOAT _ => 
				"if (" ^ s1 ^ "[" ^ s1 ^ "_len - 1] > 0.0) return 1; else\n"^
				"if (" ^ s1 ^ "[" ^ s1 ^ "_len - 1] < 0.0) return -1; else return 0"
		in
		    (code1^code^";\n", tables)
		end
	  | SignTest {num, err, comp, sign} => 
		let val (code1, s1, FLOAT, tables) = numToCString (num, tables)
		    val (code2, s2, FLOAT, tables) = numToCString (err, tables)
		    val (code3, tables) = assgmtsToCString (comp, tables)
		    val (code4, tables) = signToCString (sign, tables)
		    val code =
			"if (" ^ s1 ^ " > " ^ s2 ^ ") return 1;\n" ^ 
			"else {\n " ^
			   "if (-" ^ s1 ^ " > " ^ s2 ^ ") return -1;\n" ^ 
			   "else {\n" ^ 
			   code3^code4^"}\n}"
		in
		    (code1^code2^code^";\n", tables)
		end




    fun errToCString ((v, q), symbTable) =
	let val s = varToCString (v, symbTable)
	in
	    (s, s^" = "^(Ulps.toString q)^";\n")
	end


    fun errsToCString (nil, symbTable) = ("", "")
      | errsToCString (errs, symbTable) =
	let fun loop nil = ("", "")
	      | loop [(v, q)] = errToCString ((v, q), symbTable)
	      | loop ((v, q)::errs) = 
	        let val (decl1, code1) = errToCString ((v, q), symbTable)
		    val (decl2, code2) = loop errs
		in
		    (decl1^", "^decl2, code1^code2)
		end
	    val (decl, code) = loop errs
	in
	    ("REAL "^decl, code)
	end

    
    (* splits typeTable into lists of FLOATs, QUADs, XFLOATs *)
    (* and generates variable declarations *)

    fun declToCString (symbTable, typeTable) =
	let fun upd ((v, tp), (fl, ql, xl, xlen)) =
	    let val s = varToCString (v, symbTable)
	    in
		case tp of
		    FLOAT => (s::fl, ql, xl, xlen)
		  | QUAD => (fl, s^"[2]"::ql, xl, xlen)
		  | XFLOAT i =>
			let val s1 = if i > 10000 then s^"[10000]" else s^("["^(Int.toString i)^"]")
			    val s2 = s^"_len"
			in
			    (fl, ql, s1::xl, s2::xlen)
			end
	    end
	    fun decl (pfx, l) =
		if null l then ""
		else pfx ^ " " ^ (strListToString l)
	    val (fl, ql, xl, xlen) = VarTab.foldlIdx upd (nil, nil, nil, nil) typeTable
	in
	    (decl ("REAL", fl),
	     decl ("REAL", ql),
	     decl ("REAL", xl),
	     decl ("int", xlen))
	end





    fun toCString {name, prog=Prog {comp, errs, symbTable}} =
	let fun add (v, t) = VarTab.insert ((v, FLOAT), t)
	    fun add' ((v, _), t) = VarTab.insert ((v, FLOAT), t)
	    fun rem (v, t) = VarTab.delete (v, t)
	    fun rem' ((v, _), t) = VarTab.delete (v, t)
 
	    val typeTable = foldl add' VarTab.empty errs

	    val (decl0, code0) = errsToCString (errs, symbTable)
	    val init =
		"/*********************************************\n" ^
                " **\n" ^
		" ** Expansions limited to 10000 elements only in \n" ^
		" ** order to save memory.  This may influence the \n" ^
		" ** correctness of the results.  To avoid this, \n" ^
		" ** further analysis of the program is necessary \n" ^
		" ** in order to recycle unused space. \n" ^
		" ** \n" ^
		" ***********************************************/\n\n" ^

(*		"#include <fpu_control.h>\n" ^ *)
		"#include \"predicates.h\"\n\n" ^

		decl0 ^ ";\n\n" ^

		"void " ^ name ^ "_init () \n{\n" ^
	 (*	"int round_up = 6770, round_near = 4722;\n" ^      *)
		"double E = epsilon;\n" ^
  	 (*	"_FPU_SETCW(round_up); \n" ^ *)
	        code0 ^ 
	 (* "_FPU_SETCW(round_near); *)
		"\n}\n\n" 

	    val (vars, a, sign) =
		case comp of ([Stage(vars, a)], sign) => (vars, a, sign)
	      | _ => raise Error "not yet implemented for C target language"

	    val typeTable = foldl add typeTable vars
	    val stub = varsToCString (vars, symbTable)
	    val decl = if null vars then "" else "REAL " ^ stub

	    val (code1, tables) = assgmtsToCString (a, (symbTable, typeTable))
	    val (code2, (symbTable, typeTable)) = signToCString (sign, tables)
		
	    (* remove the input variables and errors from the declarations *)
	    val typeTable = foldl rem typeTable vars
	    val typeTable = foldl rem' typeTable errs
	    val (fs, qs, xs, xls) = declToCString (symbTable, typeTable)

	    val reservedCString = strListToString reservedNames

	    val code = 
		"int " ^ name ^ "(" ^ stub ^ ")\n" ^
		decl ^ ";\n" ^
		"{\n" ^
		"INEXACT REAL " ^ reservedCString ^ ";\n" ^
		fs ^ ";\n" ^
		qs ^ ";\n" ^
		xs ^ ";\n" ^
		xls ^ ";\n\n" ^

		code1 ^ "\n" ^
		code2 ^ "\n" ^
		"}\n"
	in
	    init^code
	end




    (*******************************************************
     ** free variables of assignment lists 
     **
     ** given a list of assignments P and a variable set V 
     ** compute (V - boundvars P) + freevars P 
     ** this would give the set of variables 
     ** that P needs to obtain in order to work and satisfy
     ** the requirements by V.
     *******************************************************)

    fun numFV (n, freeSet, boundSet) =
	case n of
	    Var v => if (VarSet.elt boundSet v) then freeSet
		     else VarSet.insert (v, freeSet)
	  | RealConst s => freeSet
          | (Square n | Uminus n | Osquare n |
	     Abs n | Double n | Approx n) => numFV(n, freeSet, boundSet)
          | (Plus (n1, n2)| Minus (n1, n2) | Times (n1, n2) | Oplus (n1, n2) | 
	     Ominus (n1, n2) | Otimes (n1, n2) | Tsquare (n1, n2)) =>
	    let val freeSet = numFV(n1, freeSet, boundSet)
	    in
		numFV(n2, freeSet, boundSet)
	    end
	  | (Tplus (n1,n2,n3) | Tminus(n1,n2,n3) | Ttimes(n1,n2,n3)) => 
	    let val freeSet = numFV(n1, freeSet, boundSet)
		val freeSet = numFV(n2, freeSet, boundSet)
	    in
		numFV(n3, freeSet, boundSet)
	    end

    fun assgmtFV (a, varSets as (freeSet, boundSet)) =
	case a of
	    Eq (v, n) => 
		let val freeSet = numFV (n, freeSet, boundSet)
		in
		    (VarSet.delete(v, freeSet), VarSet.insert (v, boundSet))
		end
	  | Susp (v, {comp, lvar, rvar}) =>
		let val (freeSet, boundSet) = assgmtsFV (comp, varSets)
		    val boundVar = VarSet.elt boundSet
		    fun f (v, set) = 
			if boundVar v then set else VarSet.insert (v, set)
		    val freeSet = foldl f freeSet (lvar @ rvar)
		in
		    (VarSet.delete (v, freeSet), VarSet.insert (v, boundSet))
		end
	  | (Lforce (v, vs) | Rforce (v, vs)) => 
		let val freeSet = 
		    if VarSet.elt boundSet v then freeSet 
		    else VarSet.insert (v, freeSet)
		in 
		    (foldl VarSet.delete freeSet vs,
		     foldl VarSet.insert boundSet vs)
		end
	    
    and assgmtsFV (aa, varSets) = foldl assgmtFV varSets aa



    fun fv (aa, freeSet) =
	#1 (assgmtsFV(aa, (freeSet, VarSet.empty)))

end
