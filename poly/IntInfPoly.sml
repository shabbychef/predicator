(**************************************
 ** IntInfPoly.sml
 ** sml
 **
 ** Quick, slow, dirty and incomplete
 ** implementation of polynomials 
 ***************************************)

structure IntInfPoly :> POLY where type R.t = IntInf.int = 
struct
    structure R = 
	struct
	    open IntInf
	    type t = int
	    val zero = fromInt 0
	    val one = fromInt 1
	    val == = op =
	    fun op / (_, _) = raise Domain
	end : NUMBER

    (* exponents are in the ascending order *)

    type poly = {coef:R.t, exp:int} list
    type t = poly

    fun toCoefExpList x = x

    fun fromNumber (x:R.t) = [{coef=x, exp=0}]
    val zero:poly = nil
	

    val one = [{coef=R.one, exp=0}]
    val X = [{coef=R.one, exp=1}]

    fun ==(x:poly, y:poly) = 
	(length x) = (length y) andalso
	ListPair.all (fn ({coef=c1, exp=e1},{coef=c2, exp=e2}) => 
		      R.==(c1, c2) andalso e1 = e2)
	(x, y)
    infix ==

    fun plus(p1, p2) = 
	let fun plus' (xs, nil, acc) = List.revAppend(acc, xs)
	      | plus' (nil, ys, acc) = List.revAppend(acc, ys)
	      | plus' (xs as (x::xt), ys as (y::yt), acc) = 
		case (Int.compare(#exp x, #exp y)) of
		    LESS => plus' (xt, ys, x::acc)
		  | GREATER => plus' (xs, yt, y::acc)
		  | EQUAL => 
			let val c = R.+(#coef x, #coef y)
			in
			    if R.==(c, R.zero) then plus'(xt, yt, acc)
			    else plus'(xt, yt, ({coef=c, exp=(#exp x)})::acc)
			end
	in
	    plus' (p1, p2, nil)
	end

    fun fromCoefExpList nil = zero
      | fromCoefExpList ((x as {coef=c, exp=e})::xt) = 
	if R.==(c, R.zero) then fromCoefExpList xt
	else if e < 0 then raise Div
	     else plus([x], (fromCoefExpList xt))
		
    fun uminus (p:poly) = map (fn {coef=c, exp=e} => {coef= R.~ c, exp=e}) p

    fun minus(p1, p2) = plus(p1, uminus p2)

    fun scale (c, nil:poly) = nil
      | scale (c, x::xs) =
	if R.==(c, R.zero) then nil
	else {coef=R.*(c, #coef x), exp= #exp x}::(scale(c, xs))

    fun scaleMon ({coef=c, exp=e}, nil:poly) = nil
      | scaleMon (m as {coef=c, exp=e}, x::xs) =
	if R.==(c, R.zero) then nil
	else {coef=R.*(c, #coef x), exp= (e + #exp x)}::(scaleMon(m, xs))
	
	
    fun times (nil, _) = nil
      | times (_, nil) = nil
      | times (x::xt, ys) = 
	let val p = times(xt, ys)
	    val q = scaleMon(x, ys)
	in
	    plus(p, q)
	end

    fun compare (nil:poly, nil:poly) = EQUAL
      | compare (x::xt, nil) = if R.<(#coef x, R.zero) then LESS else GREATER
      | compare (nil, y::yt) = if R.>(#coef y, R.zero) then LESS else GREATER
      | compare (x::xt, y::yt) = 
	case Int.compare(#exp x, #exp y) of
	    LESS => if R.<(#coef x, R.zero) then LESS else GREATER
	  | GREATER => if R.>(#coef y, R.zero) then LESS else GREATER
	  | EQUAL => (case R.compare(#coef x, #coef y) of
			  LESS => LESS
			| GREATER => GREATER
			| EQUAL => compare(xt, yt))
			

    fun max(xs, ys) = 
	case compare(xs, ys) of
	    LESS => ys
	  | _ => xs

    fun min(xs, ys) =
	case compare(xs, ys) of
	    GREATER => ys
	  | _ => xs

    fun op <= (x, y) = 
	min(x, y) == x

    fun op >= (x, y) =
	min(x, y) == y
	
    fun op < (x, y) = 
	x <= y andalso not (x == y)

    fun op > (x, y) = 
	x >= y andalso not (x == y)

    val op + = plus
    val op - = minus
    val op ~ = uminus
    val op * = times

    fun sign nil = 0
      | sign (xs:poly) = R.sign (#coef (List.last xs))


    fun toString (nil) = R.toString (R.zero)
      | toString p = 
	let fun termToString {coef=c, exp=e} = 
	    if R.==(c, R.one) then 
		if e = 0 then R.toString (R.one)
		else if e = 1 then "X"
		     else "X"^(Int.toString e)
	    else 
		if e = 0 then (R.toString c)
		else if e = 1 then (R.toString c)^"X"
		     else (R.toString c)^"X"^(Int.toString e)
	    fun toString' [x as {coef=c, exp=e}] = termToString x
	      | toString' (x::xt) = (termToString x)^" + "^(toString' xt)
	in
	    toString' p
	end

end

