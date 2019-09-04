(**********************************************
 ** Parser.sml
 ** sml
 **
 ** Aleksandar Nanevski
 **
 ** Parser for the input language 
 ** Also does alpha renaming and allocation
 ** of variables for intermediate expressions
 **********************************************)
 
structure Parser :>
sig
    exception Error
    val parse : string -> Source.prog
end
=
struct
    structure S = Source

    exception Error

    datatype exp = 
	Var of string 
      | Const of string
      | Plus of exp * exp | Minus of exp * exp 
      | Times of exp * exp | Square of exp
      | UMinus of exp 

    (* not the same as Source.prog *)
    datatype prog = Prog of {freeVars : string list, assgmts : (string * exp) list}


    datatype token = Fn | LBracket | RBracket | LParen | RParen | Let | Val | End 
      | Eq | Tilde | PlusSign | MinusSign | TimesSign | SquareSign | Arrow | Variable of string 
      | Constant of string


	
    fun tokenBreak #" " = true
      | tokenBreak #"\t" = true
      | tokenBreak #"," = true
      | tokenBreak #"\n" = true
      | tokenBreak _ = false
	

    fun tokenize nil = nil
      | tokenize (s :: ts) =
	case s of 
	    "fn" => Fn :: tokenize ts
	  | "[" => LBracket :: tokenize ts
	  | "]" => RBracket :: tokenize ts
	  | "(" => LParen :: tokenize ts
	  | ")" => RParen :: tokenize ts
	  | "let" => Let :: tokenize ts
	  | "val" => Val :: tokenize ts
	  | "end" => End :: tokenize ts
	  | "=" => Eq :: tokenize ts
	  | "~" => Tilde :: tokenize ts
	  | "+" => PlusSign :: tokenize ts
	  | "-" => MinusSign :: tokenize ts
	  | "*" => TimesSign :: tokenize ts
	  | "sq" => SquareSign :: tokenize ts
	  | "=>" => Arrow :: tokenize ts
	  | s => 
	    (case (Real.fromString s) of
		 NONE => (Variable s) :: tokenize ts
	       | SOME r => (Constant (Real.toString r)) :: tokenize ts)
	  

    fun tokenToString t =
	case t of
	    Fn => "fn"
	  | LBracket => "[" | RBracket => "]" 
	  | LParen => "(" | RParen => ")"
	  | Let => "let" | Val => "val"
	  | End => "end" | Eq => "=" | Tilde => "~"
	  | PlusSign => "+" | MinusSign => "-"
	  | TimesSign => "*" | SquareSign => "sq"
	  | Arrow => "=>" | Variable s => "variable " ^ s
	  | Constant s => "constant " ^ s
		

    fun readToken s nil =
	(print "incomplete input\n"; raise Error)
      |	readToken s (t::tokens) =
	if (s = t) then tokens 
	else
	    let val s = "expected " ^ (tokenToString s) ^ 
		" but found " ^ (tokenToString t)
	    in
		(print s; raise Error)
	    end


    fun parseVars ts = 
	let val ts = readToken LBracket ts
	    fun loop nil = (print "incomplete input\n"; raise Error)
	      | loop ((Variable s)::ts) = 
		let val (vs, ts) = loop ts
		in
		    (s::vs, ts)
		end
	      | loop (RBracket::ts) = (nil, ts)
	      | loop _ = (print "expected a variable or a bracket\n"; raise Error)
	in
	    loop ts
	end


    (* nonterminals having Prim in their name are *)
    (* not going to contain any variables or constants by their own, i.e. *)
    (* variables or constants not taking part in a unary or binary operation *)

    fun parsePrimSum nil = (print "incomplete input\n"; raise Error)
      | parsePrimSum ts = 
	let val k = parsePrimProd ts
	in
	    case k of 
		NONE =>
		let val (r, ts) = parseProd ts
		in
		    case ts of
			PlusSign :: ts =>
			    let val (r', ts) = parseSum ts
			    in
				SOME (Plus(r, r'), ts)
			    end
		      | MinusSign :: ts => SOME (parseMinus(r, ts))
		      | _ => NONE
		end
	      | SOME(r, ts) =>
		(case ts of
		     PlusSign :: ts =>
			 let val (r', ts) = parseSum ts
			 in
			     SOME (Plus(r, r'), ts)
			 end
		   | MinusSign :: ts => SOME (parseMinus(r, ts))
		   | _ => SOME (r, ts))
	end


    (* this function needed because minus associates to the left *)
    and parseMinus (r1, nil) = (print "incomplete input\n"; raise Error)
      | parseMinus (r1, ts) =
	let val (r2, ts) = parseProd ts
	in
	    case ts of 
		PlusSign :: ts =>
		    let	val (r', ts) = parseSum ts
		    in
			(Plus(Minus(r1, r2), r'), ts)
		    end
	      | MinusSign :: ts =>
		    parseMinus (Minus(r1, r2), ts)
	      | _ => (Minus(r1, r2), ts)
	end


    and parseSum nil = (print "incomplete input\n"; raise Error)
      | parseSum ts = 
	let val (r, ts) = parseProd ts
	in
	    case ts of
		PlusSign :: ts =>
		    let val (r', ts) = parseSum ts
		    in
			(Plus(r, r'), ts)
		    end
	      | MinusSign :: ts => parseMinus(r, ts)
	      | _ => (r, ts)
	end

    and parsePrimProd nil = (print "incomplete input\n"; raise Error)
      | parsePrimProd ts =
	let val k = parsePrimFactor ts
	in
	    case k of
		NONE => 
		    let val (r, ts) = parseFactor ts
		    in
			case ts of
			    TimesSign :: ts =>
				let val (r', ts) = parseProd ts
				in
				    SOME (Times(r, r'), ts)
				end
			  | _ => NONE
		    end
	      | SOME(r, ts) => 
		    (case ts of
			 TimesSign :: ts =>
			     let val (r', ts) = parseProd ts
			     in
				 SOME (Times(r, r'), ts)
			     end
		       | _ => SOME(r, ts))
	end


    and parseProd nil = (print "incomplete input\n"; raise Error)
      | parseProd ts =
	let val (r, ts) = parseFactor ts
	in
	    case ts of 
		TimesSign :: ts =>
		    let	val (r', ts) = parseProd ts
		    in
			(Times (r, r'), ts)
		    end
	      | _ => (r, ts)
	end
    

    and parsePrimFactor nil = (print "incomplete input\n"; raise Error)
      | parsePrimFactor (t::ts) =
	case t of
	    SquareSign => 
		let val (r, ts) = parseFactor ts
		in
		    SOME (Square r, ts)
		end
	  | Tilde =>
		let val (r, ts) = parseFactor ts
		in
		    SOME (UMinus r, ts)
		end
	  | LParen =>
		let val k = parsePrimSum ts
		in
		    case k of 
			NONE => NONE
		      | SOME (r, ts) => 
			    (case ts
				 of RParen :: ts => SOME (r, ts)
			       | _ => (print "right paren expected\n"; raise Error))
		end
	  | _ => NONE



    and parseFactor nil = (print "incomplete input\n"; raise Error)
      | parseFactor (t :: ts) =
	case t of
	    SquareSign => 
		let val (r, ts) = parseFactor ts
		in
		    (Square r, ts)
		end
	  | Tilde =>
		let val (r, ts) = parseFactor ts
		in
		    (UMinus r, ts)
		end
	  | LParen =>
		let val (r, ts) = parseSum ts
		in
		    case ts
			of RParen :: ts => (r, ts)
		      | _ => (print "right paren expected\n"; raise Error)
		end
	  | Variable s => (Var s, ts)
	  | Constant s => (Const s, ts)
	  | _ => (print "atom expected\n"; raise Error)


    fun parseAssgmt nil = (print "assignment expected\n"; raise Error)
      | parseAssgmt ts =
	let val (t::ts) = readToken Val ts
	in
	    case t of
		Variable s => 
		    let val ts = readToken Eq ts
			val k = parsePrimSum ts
		    in
			case k of
			    NONE =>
				(print "variables and constants cannot be assigned\n"; raise Error)
			  | SOME(e, ts) => 
				((s, e), ts)
		    end
	      | _ => raise (print "assignment expected\n"; raise Error)
	end


    fun parseAssgmts ts =
	let val ts = readToken Let ts
	    fun parse' ts = 
		let val (a, ts) = parseAssgmt ts
		in
		    case ts of
			End :: ts' => ([a], ts)
		      | Fn :: ts' => ([a], ts)
		      | _ => 
			    let val (ass, ts) = parse' ts
			    in
				(a::ass, ts) 
			    end
		end
	in
	    parse' ts
	end
    

    and parseFn nil = (print "syntax error\n"; raise Error)
      | parseFn ts =
	let val ts = readToken Fn ts
	    val (vars, ts) = parseVars ts
	    val ts = readToken Arrow ts
	    val (ass, ts) = parseAssgmts ts
	    val prg = 
		case ts of
		  End :: ts' => nil
		| Fn :: ts'  => parseFn ts
		| _ => (print "syntax error\n"; raise Error)
	in
	    
	    (Prog {freeVars = vars, assgmts = ass} :: prg)
	end


    fun parseMain s =
	let val ts = tokenize (String.tokens tokenBreak s)
	in
	    parseFn ts
	end


   
    (* compiles expression into source and allocates a variable for it *)
    fun allocVar (ss, ee, tables as (symbTable, alphaSubst, constTable, numSubst)) =
	case ee of
	    Var s => {code = nil, var = alphaSubst s, tables = tables}
	  | Const s => 
		(case (numSubst s) of
		     NONE =>
			 let val (v, symbTable) = Var.newBound (s, symbTable)
			     val c = valOf (Real.fromString s)
			     val constTable = VarTab.insert ((v, c), constTable)
			     fun numSubst' t = if (s = t) then (SOME v) else numSubst t
			     val tables = (symbTable, alphaSubst, constTable, numSubst')
			 in
			     {code = nil, var = v, tables = tables} 
			 end
		   | SOME v => {code = nil, var = v, tables = tables})
	  | _ => 
		let val (v, symbTable) = Var.newBound (ss, symbTable)
		    val {code = code, exp = e, tables = tables} = 
			allocExp (ss, ee, (symbTable, alphaSubst, constTable, numSubst))
		in
		    {code = code @ [(v, e)], var = v, tables = tables}
		end

    (* compiles expression into source without allocating variables to hold it *)
    and allocExp (ss, ee, tables) =
	case ee of
	    Plus (e1, e2) => 
		let val {code=c1, var=v1, tables=tables} = allocVar (ss, e1, tables)
		    val {code=c2, var=v2, tables=tables} = allocVar (ss, e2, tables)
		in
		    {code=(c1@c2), exp=S.Plus(v1, v2), tables=tables}
		end
	  | Minus(e1, e2) => 
		let val {code=c1, var=v1, tables=tables} = allocVar (ss, e1, tables)
		    val {code=c2, var=v2, tables=tables} = allocVar (ss, e2, tables)
		in
		    {code=(c1@c2), exp=S.Minus(v1, v2), tables=tables}
		end
	  | Times(e1, e2) => 
		let val {code=c1, var=v1, tables=tables} = allocVar (ss, e1, tables)
		    val {code=c2, var=v2, tables=tables} = allocVar (ss, e2, tables)
		in
		    {code=(c1@c2), exp=S.Times(v1, v2), tables=tables}
		end
	  | Square e1 => 
		let val {code=c1, var=v1, tables=tables} = allocVar (ss, e1, tables)
		in
		    {code=c1, exp=S.Square v1, tables=tables}
		end
	  | UMinus e1 => 
		let val {code=c1, var=v1, tables=tables} = allocVar (ss, e1, tables)
		in
		    {code=c1, exp=S.UMinus v1, tables=tables}
		end

    (* alpha renaming of the program body *)
    fun handleAssgmtList (nil, tables) = (nil, tables)
      | handleAssgmtList ((s, exp)::ts, tables) =
	let val {code=c, var=v, tables=(symbTable, alphaSubst, constTable, numSubst)} = 
	    allocVar (s, exp, tables)
	    fun alphaSubst' t = if (t = s) then v else (alphaSubst t)
	    val (c', tables) = handleAssgmtList (ts, (symbTable, alphaSubst', constTable, numSubst))
	in
	    (c @ c', tables)
	end


    (* alpha renaming of formal arguments *)
    fun handleInputVars (nil, tables) = (nil, tables)
      | handleInputVars (s::vs, (symbTable, alphaSubst, constTable, numSubst)) =
	let val (v, symbTable) = Var.newFree (s, symbTable)
	    fun alphaSubst' t = if (t = s) then v else (alphaSubst t)
	    val (vvs, tables) = handleInputVars (vs, (symbTable, alphaSubst', constTable, numSubst))
	in
	    (v::vvs, tables)
	end


    (* parse one stage *)
    fun parse' (Prog {freeVars=vss, assgmts=ass}, tables) = 
	let val (vs, tables) = handleInputVars (vss, tables)
	    val (code, tables) = handleAssgmtList (ass, tables)
	in
	    ((vs, code), tables)
	end


    fun parse s = 
	let 
	    fun loop (nil, (symbTable, _, constTable, _)) = (nil, symbTable, constTable)
	      | loop (prg :: prgs, tables) = 
		let val (sprg , tables) = parse' (prg, tables)
		    val (sprgs, symbTable, constTable) = loop (prgs, tables)
		in
		    ((sprg::sprgs), symbTable, constTable)
		end
	    val (stages, symbTable, constTable) = 
		loop (parseMain s, (Var.empty, 
				    fn s => (print ("unknown variable " ^ s ^ "\n"); raise Error),
				    VarTab.empty, fn s => NONE))
	in
	    S.Prog {stages=stages, symbTable=symbTable, constTable=constTable}
	end
    
end
