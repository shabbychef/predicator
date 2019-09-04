(**************************************
 ** UlpPoly.sml
 ** sml
 ** 
 ** Aleksandar Nanevski
 **
 ** Symbolic manipulation of machine
 ** epsilon. Outdated. Use Ulps instead
 ****************************************)

structure UlpPoly : 
sig
    include FRAC_POLY
    val E : frac
    val normalize : P.poly -> P.poly
    val approx : frac -> P.poly
end =
struct
    structure F = FracIntInfPoly
    structure P = F.P
    structure R = P.R

    val E = F.X

    val einv = IntInf.pow(IntInf.fromInt 2, 53)

    fun normalize p = 
	let fun norm' (nil, carry as {coef=cr, exp=er}, acc) = (print ("NORM''"^(Int.toString er)^"\n");
	    if er = 0 then carry::acc
	    else
		let val (q, r) = (IntInf.quot(cr, einv), IntInf.rem(cr, einv))
		in
		    if R.==(r, R.zero) then 
			norm' (nil, {coef=q, exp=er-1}, acc)
		    else
			norm' (nil, {coef=q, exp=er-1}, {coef=r, exp=er}::acc)
		end)
	      | norm' (xx as {coef=c, exp=e}::xs, carry as {coef=cr, exp=er}, acc) = 
	        if e < er then 
		    let val (q, r) = (IntInf.quot(cr, einv), IntInf.rem(cr, einv))
		    in
			if R.==(r, R.zero) then
			    norm' (xx, {coef=q, exp=er-1}, acc)
			else
			    norm' (xx, {coef=q, exp=er-1}, {coef=r, exp=er}::acc)
		    end
		else (* must be equal *)
		    let val cr = R.+(c, cr)
		    in
			if er = 0 then {coef=cr, exp=0}::acc
			else
			    let val (q, r) = (IntInf.quot(cr, einv), IntInf.rem(cr, einv))
			    in
				if R.==(r, R.zero) then
				    norm' (xs, {coef=q, exp=er-1}, acc)
				else
				    norm' (xs, {coef=q, exp=er-1}, {coef=r, exp=er}::acc)
			    end
		    end
	in
	    case rev (P.toCoefExpList p) of
		nil => P.zero
	      | x::xs => P.fromCoefExpList (norm' (xs, x, nil))
	end


    fun approx p = 
	let val {num=x, denom=y} = F.toFrac p
	    val (xx, yy) = (normalize x, normalize y)
	    val (xs, ys) = (P.toCoefExpList xx, P.toCoefExpList yy)
	    fun take2 nil = (R.zero, R.zero, ~1)
	      | take2 [{coef=a0, exp=i}] = (a0, R.zero, i)
	      | take2 ({coef=a0, exp=i}::
		       {coef=a1, exp=j}::_) = if j=i+1 then (a0, a1, i) else (a0, R.zero, i)
	    val (a0, a1, i) = take2 xs
	    val (b0, b1, j) = take2 ys
	    val p1 = 
		if R.==(b0, R.one) then 
		    P.fromCoefExpList [{coef=a0, exp=i-j}, 
				       {coef=R.-(a1, R.*(a0,b1)),exp=i-j+1}]
		else raise Domain
	    val (c0, c1, i) = take2 (P.toCoefExpList(normalize p1))
	    val p2 = P.fromCoefExpList [{coef=c0, exp=i}, {coef=c1,exp=(i+1)}]
	in
	    if F.<=(p, F.fromPoly p2) then p2
	    else
		P.fromCoefExpList [{coef=c0, exp=i},{coef=R.+(c1, R.one), exp=i+1}]
	end


    fun toString1 (nil) = R.toString (R.zero)
      | toString1 p = 
	let fun termToString {coef=c, exp=e} = 
	    if R.==(c, R.one) then 
		if e = 0 then R.toString (R.one)
		else if e = 1 then "E"
		     else "E"^(Int.toString e)
	    else 
		if e = 0 then (R.toString c)
		else if e = 1 then (R.toString c)^"E"
		     else (R.toString c)^"E"^(Int.toString e)
	    fun toString' [x as {coef=c, exp=e}] = termToString x
	      | toString' (x::xt) = (termToString x)^" + "^(toString' xt)
	in
	    toString' p
	end

    fun toString p = 
	let val {num=x1, denom=x2} = p
	in
	    (toString1 x1) ^ " / " ^ (toString1 x2)
	end

    open F

end			    
		