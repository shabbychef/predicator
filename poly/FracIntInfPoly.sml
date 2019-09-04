(***********************************
 ** FracIntInfPoly.sml
 ** sml
 **
 ** Aleksandar Nanevski
 **
 ** Rational Functions with integer
 ** coefficients
 ***********************************)


structure FracIntInfPoly :> FRAC_POLY where type R.t = IntInf.int 
					    and type P.poly = IntInfPoly.poly =
struct
    structure P = IntInfPoly
    structure R = P.R 


    type frac = {num: P.t, denom: P.t}
    type t = frac
	
    fun toFrac x = x
    fun fromFrac x = x
    fun fromNumber (x:R.t) = {num=P.fromNumber x, denom=P.one}
    fun fromPoly x = {num=x, denom=P.one}

    val zero = fromPoly P.zero
    val one = fromPoly P.one
    val X = fromPoly P.X

    fun op + ({num=x1, denom=x2}, {num=y1, denom=y2}) =
	let open P 
	in
	    {num=(x1*y2) + (x2*y1), denom=(x2*y2)}
	end

    fun op - ({num=x1, denom=x2}, {num=y1, denom=y2}) =
	let open P
	in
	    {num=(x1*y2)-(x2*y1), denom=(x2*y2)}
	end

    fun op * ({num=x1, denom=x2}, {num=y1, denom=y2}) =
	let open P
	in
	    {num=(x1*y1), denom=(x2*y2)}
	end
    
    fun op / ({num=x1, denom=x2}, {num=y1, denom=y2}) =
	let open P
	in
	    if ==(y1, zero) then raise Div 
	    else {num=(x1*y2), denom=(x2*y1)}
	end
    
    fun op ~ ({num=x1, denom=x2}) = 
	let open P
	in
	    {num= ~x1, denom=x2}
	end

    fun == ({num=x1, denom=x2}, {num=y1, denom=y2}) =
	let open P
	in
	    ==(x1*y2, x2*y1)
	end

    fun op < ({num=x1, denom=x2}, {num=y1, denom=y2}) =
	let open P
	in
	    x1*y2 < x2*y1
	end


    fun op > ({num=x1, denom=x2}, {num=y1, denom=y2}) =
	let open P
	in
	    x1*y2 > x2*y1
	end


    fun op <= ({num=x1, denom=x2}, {num=y1, denom=y2}) =
	let open P
	in
	    x1*y2 <= x2*y1
	end


    fun op >= ({num=x1, denom=x2}, {num=y1, denom=y2}) =
	let open P
	in
	    x1*y2 >= x2*y1
	end



    fun compare ({num=x1, denom=x2}, {num=y1, denom=y2}) =
	let open P
	in
	    compare(x1*y2,  x2*y1)
	end

    
    fun min (x as {num=x1, denom=x2}, y as {num=y1, denom=y2}) =
	if (x < y) then x else y


    fun max (x as {num=x1, denom=x2}, y as {num=y1, denom=y2}) =
	if (x < y) then y else x



    fun sign ({num=x1, denom=x2}) = 
	let open P
	in
	    Int.*(sign x1, sign x2)
	end


    fun toString ({num=x1, denom=x2}) =
	let open P
	in
	    (toString x1) ^ " / " ^ (toString x2)
	end
end


	

