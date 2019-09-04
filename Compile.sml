(*************************************************************************************
 ** Compile.sml
 ** sml
 ** 
 ** Aleksandar Nanevski
 **
 ** Compiler for arithmetic expressions 
 **
 ** TODO:
 ** Error analysis for unary minus not the best it can be, when the unary minus
 ** is the last assignment. Doing it nicely requires either changes to the context 
 ** structure and the rules to handle the sign test, or, better, restricting the source 
 ** language to not accept unary minus as the final operation.
 ** Anyway, it'll just unecessarily complicate things.
 **************************************************************************************)


structure Compile : COMPILE =
struct

    structure S = Source
    structure T = Target
    structure C = Context
    structure Q = Ulps

    open T
    open C

    val E = Q.E

    fun translate c v = getMatch (v, c)


    fun compileLineA ((y, e), c) =
	let val translate = translate c
	    val (ya, c) = newA (y, c)
	    val (yp, c) = newP (y, c)
	in
	    case e of
		S.Plus (x1, x2) =>
		    let val (va1, va2) = (getA(x1, c), getA(x2, c))
			val (xa1, xa2) = (translate va1, translate va2)
			val (Oa d1, Oa d2) = (getError(va1, c), getError(va2, c))
			val comp = Eq (ya, Oplus(xa1, xa2))
			val exp = Var ya
		    in
			if free(va1, c) andalso free(va2, c) then
			    let val c = putError ((ya, Oa E), c)
				val c = putMatch ((yp, P, Abs (Var ya)), c)
				val err = (Q.zero, RealConst "0.0")
			    in
				([comp], exp, err, c)
			    end
			else
			if free(va1, c) then
			    let val xp2 = translate(getP(x2, c))
				open Q
				val c = putError ((ya, Oa (E + d2)), c)
				val c = putError ((yp, P), c)
				val err = (d2 * (one + E)/(one - E), Var yp)
			    in
				([comp, Eq (yp, Oplus(Abs xa1, xp2))], 
				 exp, err, c)
			    end
			else
			if free(va2, c) then
			    let val xp1 = translate(getP(x1, c))
				open Q
				val c = putError ((ya, Oa (E + d1)), c)
				val c = putError ((yp, P), c)
				val err = (d1 * (one + E)/(one - E), Var yp)
			    in
				([comp,Eq (yp, Oplus(xp1, Abs xa2))], 
				 exp, err, c)
			    end
			else 
			let val (xp1, xp2) = (translate(getP(x1, c)), 
					      translate(getP(x2, c)))
			    open Q
			    val c = putError ((ya, Oa (E+(one+E)*max(d1,d2))), c)
			    val eb = max(d1,d2)*(one+E)*(one+E)/(one-E)
			in
			    if xp1 = xa1 andalso xp2 = xa2 then
			    let val c = putMatch ((yp, P, Var ya), c)
				val err = (eb, Var ya)
			    in
				([comp], exp, err, c)
			    end
			    else
			    let val c = putError ((yp, P), c)
				val err = (eb, Var yp)
			    in
				([comp, Eq (yp, Oplus (xp1, xp2))], exp, err, c)
			    end
			end
		    end
	      | S.Minus (x1, x2) =>
		    let val (va1, va2) = (getA(x1, c), getA(x2, c))
			val (xa1, xa2) = (translate va1, translate va2)
			val (Oa d1, Oa d2) = (getError(va1, c), getError(va2, c))
			val comp = Eq (ya, Ominus(xa1, xa2))
			val exp = Var ya
		    in
			if free(va1, c) andalso free(va2, c) then
			    let val c = putError ((ya, Oa E), c)
				val c = putMatch ((yp, P, Abs (Var ya)), c)
				val err = (Q.zero, RealConst "0.0")
			    in
				([comp], exp, err, c)
			    end
			else
			if free(va1, c) then
			    let val xp2 = translate(getP(x2, c))
				open Q
				val c = putError ((ya, Oa (E + d2)), c)
				val c = putError ((yp, P), c)
				val err = (d2 * (one + E)/(one - E), Var yp)
			    in
				([comp, Eq (yp, Oplus(Abs xa1, xp2))], 
				 exp, err, c)
			    end
			else
			if free(va2, c) then
			    let val xp1 = translate(getP(x1, c))
				open Q
				val c = putError ((ya, Oa (E + d1)), c)
				val c = putError ((yp, P), c)
				val err = (d1 * (one + E)/(one - E), Var yp)
			    in
				([comp,Eq (yp, Oplus(xp1, Abs xa2))], 
				 exp, err, c)
			    end
			else 
			let val (xp1, xp2) = (translate(getP(x1, c)), translate(getP(x2, c)))
			    open Q
			    val c = putError ((ya, Oa (E+(one+E)*max(d1,d2))), c)
			    val c = putError ((yp, P), c)
			    val err = (max(d1,d2)*(one+E)*(one+E)/(one-E), Var yp)
			in
			    ([comp, Eq (yp, Oplus (xp1, xp2))], exp, err, c)
			end
		    end
	      | S.Times(x1, x2) => 
		    let val (va1, va2) = (getA(x1, c), getA(x2, c))
			val (xa1, xa2) = (translate va1, translate va2)
			val (Oa d1, Oa d2) = (getError(va1, c), getError(va2, c))
			val comp = Eq (ya, Otimes(xa1, xa2))
			val exp = Var ya
		    in
			if free(va1, c) andalso free(va2, c) then
			    let val c = putError ((ya, Oa E), c)
				val c = putMatch ((yp, P, Abs (Var ya)), c)
				val err = (Q.zero, RealConst "0.0")
			    in
				([comp], exp, err, c)
			    end
			else
			if free(va1, c) then
			    let val xp2 = translate(getP(x2, c))
				open Q
				val c = putError ((ya, Oa (E+(one+E)*d2)), c)
				val eb = d2*(one+E)*(one+E)/(one - E)
			    in
				if xp2 = xa2 orelse xp2 = Abs xa2 then
				    let val c = putMatch ((yp, P, Abs(Var ya)), c)
				    in
					([comp], exp, (eb, Abs(Var ya)), c)
				    end
				else
				    let val c = putError ((yp, P), c)
				    in
					([comp, Eq (yp, Otimes(Abs xa1, xp2))],
					 exp, (eb, Var yp), c)
				    end
			    end
			else
			if free(va2, c) then
			    let val xp1 = translate(getP(x1, c))
				open Q
				val c = putError ((ya, Oa (E+(one+E)*d1)), c)
				val eb = d1*(one+E)*(one+E)/(one - E)
			    in
				if xp1 = xa1 orelse xp1 = Abs xa1 then
				    let val c = putMatch ((yp, P, Abs(Var ya)), c)
				    in
					([comp], exp, (eb, Abs(Var ya)), c)
				    end
				else
				    let val c = putError ((yp, P), c)
				    in
					([comp, Eq (yp, Otimes(xp1, Abs xa2))],
					 exp, (eb, Var yp), c)
				    end
			    end
			else
			let val (xp1, xp2) = (translate(getP(x1, c)), translate(getP(x2, c)))
			    open Q
			    val c = putError ((ya, Oa (E+(one+E)*(d1+d2+d1*d2))), c)
			    val eb = (one+E)*(one+E)*(d1+d2+d1*d2)/(one-E)
			in
			    if xp1 = xa1 andalso xp2 = xa2 then
			    let val c = putMatch ((yp, P, Var ya), c)
				val err = (eb, Var ya)
			    in
				([comp], exp, err, c)
			    end
			    else
		            if ((xp1 = xa1 orelse xp1 = Abs xa1) andalso
				(xp2 = xa2 orelse xp2 = Abs xa2)) then
			    let val c = putMatch ((yp, P, Abs(Var ya)), c)
				val err = (eb, Var ya)
			    in
				([comp], exp, err, c)
			    end
			    else
			    let val c = putError ((yp, P), c)
				val err = (eb, Var yp)
			    in
				([comp, Eq (yp, Otimes (xp1, xp2))], exp, err, c)
			    end
			end
		    end
	      | S.Square x => 
		    let val va = getA(x, c)
			val xa = translate va
			val Oa d = getError (va, c)
			val comp = Eq (ya, Osquare xa)
			val exp = Var ya
		    in
			if free(va, c) then
			    let val c = putError ((ya, Oa E), c)
				val c = putMatch ((yp, P, Var ya), c)
				val err = (Q.zero, RealConst "0.0")
			    in
				([comp], exp, err, c)
			    end
			else
			let val xp = translate(getP(x, c))
			    open Q
			    val c = putError ((ya, Oa (E+(one+E)*(d+d+d*d))), c)
			    val eb = (one+E)*(one+E)*(d+d+d*d)/(one-E)
			in
			    if xp = xa orelse xp = Abs xa then
			    let val c = putMatch ((yp, P, Var ya), c)
				val err = (eb, Var ya)
			    in
				([comp], exp, err, c)
			    end
			    else
			    let val c = putError ((yp, P), c)
				val err = (eb, Var yp)
			    in
				([comp, Eq(yp, Osquare xp)], exp, err, c)
			    end
			end
		    end
	      | S.UMinus x => 
		    let val va = getA(x, c)
			val xa = translate va
			val Oa d = getError (va, c)
			val c = putError ((ya, Oa d), c)
			val comp = Eq (ya, Uminus xa)
		    in
			if free(va, c) then
			    let val err = (Q.zero, RealConst "0.0")
			    in
				([comp], RealConst "0.0", err, c)
			    end
			else
			    let val xp = translate(getP(x, c))
				val c = putMatch ((yp, P, xp), c)
				val err = (d, xp)
			    in
				([comp], Var ya, err, c)
			    end
		    end
	end






    fun compileLineB ((y, e), c) =
	let val (yb, c) = newB (y, c)
	    val exp = Approx (Var yb)
	    val translate = translate c
	in
	    case e of
		S.Plus (x1, x2) =>
		    let val (va1, va2) = (getA (x1, c), getA (x2, c))
			val (xa1, xa2) = (translate va1, translate va2)
		    in
			if free(va1, c) andalso free(va2, c) then 
			    let val c = putMatch ((yb, Ob E, translate(getA(y, c))), c)
				val err = (Q.zero, RealConst "0.0")
			    in
				(* if in last stage, this is dead code, so don't *)
				(* bother to put correct end expreesion *)
				([], RealConst "0.0", err, c) 
			    end
			else
			if free(va1, c) then
			    let val vb2 = getB(x2, c)
				val xb2 = translate vb2
				val Ob d2 = getError (vb2, c)
				val xp2 = translate(getP(x2, c))
				open Q
				val c = putError ((yb, Ob d2), c)
				val err = (d2*(one+E)/(one-E-E), xp2)
			    in
				([Eq (yb, Plus(xa1, xb2))], exp, err, c)
			    end
			else
			if free(va2, c) then
			    let val vb1 = getB(x1, c)
				val xb1 = translate vb1
				val Ob d1 = getError (vb1, c)
				val xp1 = translate(getP(x1, c))
				open Q
				val c = putError ((yb, Ob d1), c)
				val err = (d1*(one+E)/(one-E-E), xp1)
			    in
				([Eq (yb, Plus(xb1, xa2))], exp, err, c)
			    end
			else 
			    let val (vb1, vb2) = (getB(x1, c), getB(x2, c))
				val (xb1, xb2) = (translate vb1, translate vb2)
				val (Ob d1, Ob d2) = (getError(vb1, c), getError(vb2, c))
				val yp = translate(getP(y, c))
				open Q
				val eb = (one+E)*max(d1,d2)
				val c = putError ((yb, Ob eb), c)
				val err = (eb*(one+E)/(one-E-E), yp)
			    in
				([Eq (yb, Plus(xb1, xb2))], exp, err, c)
			    end
		    end
	      | S.Minus (x1, x2) =>
		    let val (va1, va2) = (getA(x1, c), getA(x2, c))
			val (xa1, xa2) = (translate va1, translate va2)
		    in
			if free(va1, c) andalso free(va2, c) then 
			    let val c = putMatch ((yb, Ob E, translate(getA(y,c))), c)
				val err = (Q.zero, RealConst "0.0")
			    in
				(* if in last stage, this is dead code *)
				([], RealConst "0.0", err, c) 
			    end
			else
			if free(va1, c) then
			    let val vb2 = getB(x2, c)
				val xb2 = translate vb2
				val Ob d2 = getError (vb2, c)
				val xp2 = translate(getP(x2, c))
				open Q
				val c = putError ((yb, Ob d2), c)
				val err = (d2*(one+E)/(one-E-E), xp2)
			    in
				([Eq (yb, Minus(xa1, xb2))], exp, err, c)
			    end
			else
			if free(va2, c) then
			    let val vb1 = getB (x1, c)
				val xb1 = translate vb1
				val Ob d1 = getError (vb1, c)
				val xp1 = translate(getP(x1, c))
				open Q
				val c = putError ((yb, Ob d1), c)
				val err = (d1*(one+E)/(one-E-E), xp1)
			    in
				([Eq (yb, Minus(xb1, xa2))], exp, err, c)
			    end
			else 
			    let val (vb1, vb2) = (getB(x1, c), getB(x2, c))
				val (xb1, xb2) = (translate vb1, translate vb2)
				val (Ob d1, Ob d2) = (getError(vb1, c), getError(vb2, c))
				val yp = translate(getP(y, c))
				open Q
				val eb = (one+E)*max(d1,d2)
				val c = putError ((yb, Ob eb), c)
				val err = (eb*(one+E)/(one-E-E), yp)
			    in
				([Eq (yb, Minus(xb1, xb2))], exp, err, c)
			    end
		    end
	      | S.Times(x1, x2) => 
		    let val (va1, va2) = (getA(x1, c), getA(x2, c))
			val (xa1, xa2) = (translate va1, translate va2)
			val yp = translate(getP(y, c))
		    in
			if free(va1, c) andalso free(va2, c) then
			    let val c = putMatch ((yb, Ob E, translate(getA(y, c))), c)
				val err = (Q.zero, RealConst "0.0")
			    in
				([], RealConst "0.0", err, c)
			    end
			else
			if free(va1, c) then
			    let val vb2 = getB(x2, c)
				val xb2 = translate vb2
				val Ob d2 = getError(vb2, c)
				open Q
				val c = putError ((yb, Ob ((one+E)*d2)), c)
				val err = (d2*(one+E)*(one+E)/(one-E-E), yp)
			    in
				([Eq (yb, Times(xa1, xb2))], exp, err, c)
			    end
			else 
			if free(va2, c) then
			    let val vb1 = getB(x1, c)
				val xb1 = translate vb1
				val Ob d1 = getError(vb1, c)
				open Q
				val c = putError ((yb, Ob ((one+E)*d1)), c)
				val err = (d1*(one+E)*(one+E)/(one-E-E), yp)
			    in
				([Eq (yb, Times(xb1, xa2))], exp, err, c)
			    end
			else
			    let val (vb1, vb2) = (getB(x1, c), getB(x2, c))
				val (xb1, xb2) = (translate vb1, translate vb2)
				val (Ob d1, Ob d2) = (getError(vb1, c), getError(vb2, c))
				open Q
				val eb = (one+E)*(d1+d2+d1*d2)
				val c = putError ((yb, Ob eb), c)
				val err = ((one+E)*eb/(one-E-E), yp)
			    in
				([Eq (yb, Times(xb1, xb2))], exp, err, c)
			    end
		    end
	      | S.Square x => 
		    let val va = getA(x, c)
			val xa = translate va
			val yp = translate(getP(x, c))
		    in
			if free(va, c) then
			    let val c = putMatch((yb, Ob E, translate(getA(y, c))), c)
				val err = (Q.zero, RealConst "0.0")
			    in
				([], RealConst "0.0", err, c)
			    end
			else
			let val vb = getB(x, c)
			    val xb = translate vb
			    val Ob d = getError(vb, c)
			    open Q
			    val eb = (one+E)*(d+d+d*d)
			    val c = putError((yb, Ob eb), c)
			    val err = ((one+E)*eb/(one-E-E), yp)
			in
			    ([Eq (yb, Square xb)], exp, err, c)
			end
		    end
	      | S.UMinus x => 
		    let val va = getA(x, c)
			val xa = translate va
		    in
			if free(va, c) then
			    let val c = putMatch((yb, Ob Q.zero, translate(getA(y, c))), c)
				val err = (Q.zero, RealConst "0.0")
			    in
				([], exp, err, c)
			    end
			else
			    let val vb = getB(x, c)
				val xb = translate vb
				val Ob d = getError (vb, c)
				val yp = translate(getP(y, c))
				val c = putError ((yb, Ob d), c)
				open Q
				val err = (d*(one+E)/(one-E-E), yp)
			    in
				([Eq (yb, Uminus xb)], exp, err, c)
			    end
		    end
	end
    





    fun compileLineC ((y, e), c) =
	let val (yc, c) = newC (y, c)
	    val translate = translate c
	    fun rhoPlus(r1, r2) = 
		let open Q 
		in
		    if Q.==(r1, zero) then E + r2
		    else if Q.==(r2, zero) then E + r1
			 else E + (one + E)*max(r1, r2)
		end
	in
	    case e of
		S.Plus (x1, x2) =>
		    let val (va1, va2) = (getA (x1, c), getA (x2, c))
			val (xa1, xa2) = (translate va1, translate va2)
		    in
			if free(va1, c) andalso free(va2, c) then 
			    let val c = putError ((yc, Oc(Q.zero, E, Q.zero)), c)
				val ya = translate(getA(y, c))
				val err = (Q.zero, RealConst "0.0")
			    in
				(* if in last stage, this is dead code *)
				([Eq (yc, Tplus(xa1, xa2, ya))], RealConst "0.0", err, c) 
			    end
			else
			if free(va1, c) then
			    let val vc2 = getC(x2, c)
				val xc2 = translate vc2
				val Oc (D2 as (d2, _, _)) = getError(vc2, c)
				val xp2 = translate(getP(x2, c))
				open Q
				val c = putMatch ((yc, Oc D2, xc2), c)
				val err = (d2*(one+E)*(one+E)/(one-E), xp2)
			    in
				([], xc2, err, c)
			    end
			else
			if free(va2, c) then
			    let val vc1 = getC(x1, c)
				val xc1 = translate vc1
				val Oc (D1 as (d1, _, _)) = getError(vc1, c)
				val xp1 = translate(getP(x1, c))
				open Q
				val c = putMatch ((yc, Oc D1, xc1), c)
				val err = (d1*(one+E)*(one+E)/(one-E), xp1)
			    in
				([], xc1, err, c)
			    end
			else
			    let val (vc1, vc2) = (getC(x1, c), getC(x2, c))
				val (xc1, xc2) = (translate vc1, translate vc2)
				val yp = translate(getP(y, c))
				val (Oc D1, Oc D2) = (getError(vc1, c), getError(vc2, c))
				val ((d1,i1,r1), (d2,i2,r2)) = (D1, D2)
				open Q
				val dc = (one+E)*(E*max(i1,i2)+max(d1,d2))
				val DC = (dc, max(i1,i2)*(one+E)/(one-E), rhoPlus(r1, r2))
				val c = putError ((yc, Oc DC), c)
				val err = (dc*(one+E)*(one+E)/(one-E), yp)
			    in
				([Eq (yc, Oplus(xc1, xc2))], Var yc, err, c)
			    end
		    end
	      | S.Minus (x1, x2) =>
		    let val (va1, va2) = (getA (x1, c), getA (x2, c))
			val (xa1, xa2) = (translate va1, translate va2)
		    in
			if free(va1, c) andalso free(va2, c) then 
			    let val c = putError ((yc, Oc(Q.zero, E, Q.zero)), c)
				val ya = translate(getA(y, c))
				val err = (Q.zero, RealConst "0.0")
			    in
				(* if in last stage, this is dead code *)
				([Eq (yc, Tminus(xa1, xa2, ya))], RealConst "0.0", err, c) 
			    end
			else
			if free(va1, c) then
			    let val vc2 = getC(x2, c)
				val xc2 = translate vc2
				val xp2 = translate(getP(x2, c))
				val Oc (D2 as (d2, _, _)) = getError(vc2, c)
				val c = putError ((yc, Oc D2), c)
				open Q
				val err = (d2*(one+E)*(one+E)/(one-E), xp2)
			    in
				([Eq (yc, Uminus xc2)], Var yc, err, c)
			    end
			else
			if free(va2, c) then
			    let val vc1 = getC(x1, c)
				val xc1 = translate vc1
				val Oc (D1 as (d1, _, _)) = getError(vc1, c)
				val xp1 = translate(getP(x1, c))
				open Q
				val c = putMatch ((yc, Oc D1, xc1), c)
				val err = (d1*(one+E)*(one+E)/(one-E), xp1)
			    in
				([], xc1, err, c)
			    end
			else
			    let val (vc1, vc2) = (getC(x1, c), getC(x2, c))
				val (xc1, xc2) = (translate vc1, translate vc2)
				val yp = translate(getP(y, c))
				val (Oc D1, Oc D2) = (getError(vc1, c), getError(vc2, c))
				val ((d1,i1,r1), (d2, i2, r2)) = (D1, D2)
				open Q
				val dc = (one+E)*(E*max(i1,i2)+max(d1,d2))
				val DC = (dc, max(i1,i2)*(one+E)/(one-E), rhoPlus(r1,r2))
				val c = putError ((yc, Oc DC), c)
				val err = (dc*(one+E)*(one+E)/(one-E), yp)
			    in
				([Eq (yc, Ominus(xc1, xc2))], Var yc, err, c)
			    end
		    end
	      | S.Times(x1, x2) => 
		    let val (va1, va2) = (getA(x1, c), getA(x2, c))
			val (xa1, xa2) = (translate va1, translate va2)
			val yp = translate(getP(y, c))
		    in
			if free(va1, c) andalso free(va2, c) then
			    let val c = putError ((yc, Oc (Q.zero, E, Q.zero)), c)
				val ya = translate(getA(y, c))
				val err = (Q.zero, RealConst "0.0")
			    in
				([Eq (yc, Ttimes(xa1, xa2, ya))], RealConst "0.0", err, c)
			    end
			else
			if free(va1, c) then
			    let val vc2 = getC(x2, c)
				val xc2 = translate vc2
				val Oc (D2 as (d2,i2,r2)) = getError(vc2, c)
				open Q
				val dc = E*i2+d2
				val DC = (dc, i2*(one+E)/(one-E), E+(one+E)*r2)
				val c = putError ((yc, Oc DC), c)
				val err = (dc*(one+E)*(one+E)/(one-E), yp)
			    in
				([Eq (yc, Otimes(xa1, xc2))], Var yc, err, c)
			    end
			else 
			if free(va2, c) then
			    let val vc1 = getC(x1, c)
				val xc1 = translate vc1
				val Oc (D1 as (d1,i1,r1)) = getError(vc1, c)
				open Q
				val dc = E*i1+d1
				val DC = (dc, i1*(one+E)/(one-E), E+(one+E)*r1)
				val c = putError ((yc, Oc DC), c)
				val err = (dc*(one+E)*(one+E)/(one-E), yp)
			    in
				([Eq (yc, Otimes(xc1, xa2))], Var yc, err, c)
			    end
			else 
			    let val (vc1, vc2) = (getC(x1, c), getC(x2, c))
				val (xc1, xc2) = (translate vc1, translate vc2)
				val (Oc D1, Oc D2) = (getError(vc1, c), getError(vc2, c))
				val ((d1,i1,r1),(d2,i2,r2)) = (D1, D2)
				open Q
				val dc = ((E+E+E*E)*(i1+i2)+(r1*i2+i1*r2)+
					  d1*(one+i2+r2)+d2*(one+i1+r1)+i1*i2+d1*d2)*(one+E)
				val DC = (dc, (i1+i2)*(one+E)/(one-E-E-E*E), E+(one+E)*(r1+r2+r1*r2))
				val c = putError ((yc, Oc DC), c)
				val err = (dc*(one+E)*(one+E)/(one-E), yp)
			    in
				([Eq (yc, Oplus(Otimes(xa1, xc2), Otimes(xc1, xa2)))], Var yc, err, c)
			    end
		    end
	      | S.Square x => 
		    let val va = getA(x, c)
			val xa = translate va
			val yp = translate(getP(y, c))
		    in
			if free(va, c) then
			    let val c = putError ((yc, Oc (Q.zero, E, Q.zero)), c)
				val ya = translate(getA(y, c))
				val err = (Q.zero, RealConst "0.0")
			    in
				([Eq (yc, Tsquare(xa, ya))], RealConst "0.0", err, c)
			    end
			else
			let val vc = getC(x, c)
			    val xc = translate vc
			    val Oc (D as (d,i,r)) = getError(vc, c)
			    open Q
			    val dc = (one+E)*((one+one)*(E*i+i*r+d*(one+r+i))+(i*i+d*d))
			    val DC = (dc, (one+E)*(i+i)/(one-E), E+(one+E)*(r+r+r*r))
			    val c = putError ((yc, Oc DC), c)
			    val err = (dc*(one+E)*(one+E)/(one-E), yp)
			in
			    ([Eq (yc, Double(Otimes(xa, xc)))], Var yc, err, c)
			end
		    end
	      | S.UMinus x => 
		    let val va = getA(x, c)
			val xa = translate va
			val yp = translate(getP(y, c))
		    in
			if free(va, c) then
			    let val c = putMatch((yc, Oc (Q.zero, E, Q.zero), translate(getA(y, c))), c)
				val err = (Q.zero, RealConst "0.0")
			    in
				([], RealConst "0.0", err, c)
			    end
			else
			    let val vc = getC(x, c)
				val xc = translate vc
				val Oc (D as (d,i,r)) = getError (vc, c)
				val c = putError ((yc, Oc D), c)
				open Q
				val err = (d*(one+E)*(one+E)/(one-E), yp)
			    in
				([Eq (yc, Uminus xc)], Var yc, err, c)
			    end
		    end
	end




    fun compileLineD ((y, e), c) =
	let val (yd, c) = newD (y, c)
	    val translate = translate c
	    val yb = translate(getB(y, c))
	in
	    case e of
		S.Plus (x1, x2) =>
		    let val (va1, va2) = (getA (x1, c), getA (x2, c))
		    in
			if free(va1, c) andalso free(va2, c) then 
			    let val yc = translate(getC(y, c))
				val c = putMatch((yd, Od, yc), c)
			    in
				(* if in last stage, this is dead code *)
				([], RealConst "0.0", c) 
			    end
			else
			if free(va1, c) then
			    let val xd2 = translate(getD(x2, c))
				val c = putMatch ((yd, Od, xd2), c)
			    in
				([], Plus(yb, xd2), c)
			    end
			else
			if free(va2, c) then
			    let val xd1 = translate(getD(x1, c))
				val c = putMatch ((yd, Od, xd1), c)
			    in
				([], Plus(yb, xd1), c)
			    end
			else
			    let val (xd1, xd2) = (translate(getD(x1, c)), translate(getD(x2, c)))
				val c = putError ((yd, Od), c)
			    in
				([Eq (yd, Plus(xd1, xd2))], Plus(yb, Var yd), c)
			    end
		    end
	      |	S.Minus (x1, x2) =>
		    let val (va1, va2) = (getA (x1, c), getA (x2, c))
		    in
			if free(va1, c) andalso free(va2, c) then 
			    let val yc = translate(getC(y, c))
				val c = putMatch((yd, Od, yc), c)
			    in
				(* if in last stage, this is dead code *)
				([], RealConst "0.0", c) 
			    end
			else
			if free(va1, c) then
			    let val xd2 = translate(getD(x2, c))
				val c = putError ((yd, Od), c)
			    in
				([Eq (yd, Uminus xd2)], Plus(yb, Var yd), c)
			    end
			else
			if free(va2, c) then
			    let val xd1 = translate(getD(x1, c))
				val c = putMatch ((yd, Od, xd1), c)
			    in
				([], Plus(yb, xd1), c)
			    end
			else
			    let val (xd1, xd2) = (translate(getD(x1, c)), translate(getD(x2, c)))
				val c = putError ((yd, Od), c)
			    in
				([Eq (yd, Minus(xd1, xd2))], Plus(yb, Var yd), c)
			    end
		    end
	      | S.Times(x1, x2) => 
		    let val (va1, va2) = (getA(x1, c), getA(x2, c))
			val (xa1, xa2) = (translate va1, translate va2)
		    in
			if free(va1, c) andalso free(va2, c) then
			    let val yc = translate(getC(y, c))
				val c = putMatch ((yd, Od, yc), c)
			    in
				([], RealConst "0.0", c)
			    end
			else
			if free(va1, c) then
			    let val xd2 = translate(getD(x2, c))
				val c = putError((yd, Od), c)
			    in
				([Eq (yd, Times(xa1, xd2))], Plus(yb, Var yd), c)
			    end
			else 
			if free(va2, c) then
			    let val xd1 = translate(getD(x1, c))
				val c = putError((yd, Od), c)
			    in
				([Eq (yd, Times(xd1, xa2))], Plus(yb, Var yd), c)
			    end
			else 
			    let val (xd1, xd2) = (translate(getD(x1, c)), translate(getD(x2, c)))
				val (xb1, xb2) = (translate(getB(x1, c)), translate(getB(x2, c)))
				val c = putError ((yd, Od), c)
			    in
				([Eq (yd, Plus(Plus(Times(xb1, xd2), 
						    Times(xd1, xb2)),
					       Times(xd1, xd2)))], Plus(yb, Var yd), c)
			    end
		    end
	      | S.Square x => 
		    let val va = getA(x, c)
		    in
			if free(va, c) then
			    let val yc = translate(getC(y, c))
				val c = putMatch ((yd, Od, yc), c)
			    in
				([], RealConst "0.0", c)
			    end
			else
			let val xb = translate(getB(x, c))
			    val xd = translate(getD(x, c))
			    val c = putError ((yd, Od), c)

			in
			    ([Eq (yd, Plus(Double(Times(xb, xd)), Square xd))], 
			     Plus(yb, Var yd), c)
			end
		    end
	      | S.UMinus x => 
		    let val va = getA(x, c)
		    in
			if free(va, c) then
			    let val c = putMatch((yd, Od, RealConst "0.0"), c)
			    in
				([], RealConst "0.0", c)
			    end
			else
			    let val xd = translate(getD(x, c))
				val c = putError ((yd, Od), c)
			    in
				([Eq (yd, Uminus xd)], Plus(yb, Var yd), c)
			    end
		    end
	end
	 


    
    fun compileA ([(y, e)], c) = compileLineA ((y, e), c)
      | compileA ((y, e)::t, c) =
	let val (code0, _, _, c) = compileLineA ((y, e), c)
	    val (code1, exp, err, c) = compileA (t, c)
	in
	    (code0 @ code1, exp, err, c)
	end


    fun compileB ([(y, e)], c) = compileLineB ((y, e), c)
      | compileB ((y, e)::t, c) =
	let val (code0, _, _, c) = compileLineB ((y, e), c)
	    val (code1, exp, err, c) = compileB (t, c)
	in
	    (code0 @ code1, exp, err, c)
	end


    fun compileC ([(y, e)], c) = compileLineC ((y, e), c)
      | compileC ((y, e)::t, c) =
	let val (code0, _, (err, perm), c) = compileLineC ((y, e), c)
	    val (code1, exp, err, c) = compileC (t, c)
	in
	    (code0 @ code1, exp, err, c)
	end

    fun compileD ([(y, e)], c) = compileLineD ((y, e), c)
      | compileD ((y, e)::t, c) =
	let val (code0, _, c) = compileLineD ((y, e), c)
	    val (code1, exp, c) = compileD (t, c)
	in
	    (code0 @ code1, exp, c)
	end
    





    fun contextInit (freeVars, c) =
	let fun init (v, (l, c)) =
	    let val (v', c) = newA (v, c)
	    in
		(v'::l, putError ((v', Oa Q.zero), c))
	    end
	in
	    foldr init ([], c) freeVars 
	end


    fun compileStages ([(freeVars, comp)], c, susps as (sb, sc, sd)) =
	let val (targetVars, c) = contextInit (freeVars, c)
	    val (compA, expA, (erA, permA), c) = compileA (comp, c)
	    val (compB, expB, (erB, permB), c) = compileB (comp, c)
	    val (compC, expC, (erC, permC), c) = compileC (comp, c)
	    val (compD, expD, c) = compileD (comp, c)

	    (* compute the varsets required by each phase *)
	    val sdVars = fv (compD, VarSet.empty)
	    val scVars = fv (compC, sdVars)
	    val sbVars = fv (compB, scVars)

	    (* compute the actual variable lists for the suspensions and forces *)
	    val leftD = domD (sdVars,c)
	    val leftC = domC (scVars,c)
	    val leftB = domB (sbVars,c)


	    (* new variables for the tested values and errors *)
	    val (yae, c) = new ("yAE", c)
	    val (ybx, c) = new ("yBX", c)
	    val (ybe, c) = new ("yBE", c)
	    val (ycx, c) = new ("yCX", c)
	    val (yce, c) = new ("yCE", c)

	    val (eA, c) = new ("errA", c)
	    val (eB, c) = new ("errB", c)
	    val (eC, c) = new ("errC", c)
	    val (eCX, c) = new ("errCX", c)

	    val (errA, errB, errC, errCX) = (Otimes (Var eA, permA), Otimes(Var eB, permB),
					     Otimes (Var eC, permC), Otimes(Var eCX, Var ybx))

	    val stage = Stage (targetVars, compA @ [Eq (yae, errA)])

	    val erCX = let open Q in (E+E)*(one+E)*(one+E)/(one-E) end
	    val errors = [(eA, erA), (eB, erB), (eC, erC), (eCX, erCX)]

	    val sign = 
		SignTest {num = expA, err = Var yae, 
			  comp = (Rforce (sb, VarSet.toList leftB) :: compB) @ 
			         [Eq (ybx, expB), Eq (ybe, errB)],
			  sign = 
		SignTest {num = Var ybx, err = Var ybe,
			  comp = (Rforce (sc, VarSet.toList leftC) :: compC) @
			         [Eq (ycx, Oplus(Var ybx, expC)),
				  Eq (yce, Oplus(errCX, errC))],
		         sign = 
	        SignTest {num = Var ycx, err = Var yce,
			  comp = Rforce (sd, VarSet.toList leftD) :: compD,
			  sign = Sign expD}}}
						  
	in
	    ([stage], sign, (leftB, leftC, leftD), errors, c)
	end
      | compileStages ((freeVars, comp)::ss, c, susps as (sb, sc, sd)) =
	let val (targetVars, c) = contextInit (freeVars, c)
	    val (compA, _, _, c) = compileA (comp, c)
	    val (compB, _, _, c) = compileB (comp, c)
	    val (compC, _, _, c) = compileC (comp, c)
	    val (compD, _, c) = compileD (comp, c)

	    (* compile the next stages and get the requested variables *)
	    val (nsb, c) = new ("suspB", c)
	    val (nsc, c) = new ("suspC", c)
	    val (nsd, c) = new ("suspD", c)
		
	    val (stages, sign, (nsbVars, nscVars, nsdVars), errors, c) = 
		compileStages (ss, c, (nsb, nsc, nsd))

	    (* compute the variables required by phases of this stage *)
	    val sdVars = fv (compD, nsdVars)
	    val scVars = fv (compC, VarSet.union(sdVars, nscVars))
	    val sbVars = fv (compB, VarSet.union(scVars, nsbVars))

	    (* compute the actual variable lists for the suspensions and forces *)

	    val (leftD, upDC, upDB) = (domD (sdVars,c), domC(sdVars,c), domB(sdVars,c))
	    (* no need for up here, but just in case *)
	    val (leftC, upC) = (domC(scVars,c), domB(scVars,c))
	    val leftB = domB(sbVars,c)

	    val comp =
		compA @
		[Susp (nsb, {comp = Rforce (sb, VarSet.toList leftB) :: compB, 
			     lvar = VarSet.toList upDB, rvar = VarSet.toList nsbVars}),
		 Susp (nsc, {comp = Rforce (sc, VarSet.toList leftC) :: compC, 
			     lvar = VarSet.toList upDC, rvar = VarSet.toList nscVars}),
		 Susp (nsd, {comp = Rforce (sd, VarSet.toList leftD) ::
			            Lforce (nsb, VarSet.toList upDB) :: 
			            Lforce (nsc, VarSet.toList upDC) :: compD,
				    lvar = [], rvar = VarSet.toList nsdVars})]
	    val stage = Stage (targetVars, comp)	

	in
	    (stage::stages, sign, (leftB, leftC, leftD), errors, c)
	end






    fun compile (S.Prog{stages, constTable, symbTable}) =
	let fun init ((v, _), c) =
	    let val (v', c) = newA (v, c)
		val s = Real.toString (valOf (VarTab.find (v, constTable)))
	    in 
		putMatch ((v', Oa Q.zero, RealConst s), c)
	    end 
	    val c = foldl init empty (VarTab.toList constTable)

	    val (sb, c) = new ("suspB", c)
	    val (sc, c) = new ("suspC", c)
	    val (sd, c) = new ("suspD", c)

	    val (stages, sign, _, errs, c) = compileStages (stages, c, (sb, sc, sd))
	in
	    Prog {comp=(stages, sign), errs=errs, symbTable=getVars c}
	end

    
    val parse = Parser.parse

    fun generate prog =	T.toString (compile (parse prog))
    fun generateC {name, prog} = T.toCString {name=name, prog=compile(parse prog)}

    fun install {name, prog} =
	let val prog = T.toString (compile (parse prog))
	    val pprog = "local structure R = Real\n"^
		        "      structure Q = ExtendedFloat\n"^
		        "      structure X = XFloat\n"^
			"      fun susp f = \n" ^
			"          let val x = ref (fn () => raise Domain) \n" ^
			"          in \n" ^
			"              x := (fn () => let val c = f() \n" ^
			"                             in \n" ^
			"                                 c before \n" ^
			"                                 x := (fn () => c) \n" ^
			"                             end); \n" ^
			"              fn () => (!x)()\n" ^
			"          end \n"^
			"in\n"^
			"val " ^ name ^ " = " ^ prog ^ "end (* local *)\n"
	in
	    Compiler.Interact.useStream(TextIO.openString pprog)
	end
end
