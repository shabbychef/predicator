(*************************************
 ** Ulps.sml
 ** sml
 **
 ** Aleksandar Nanevski
 **
 ** Symbolic operations over the 
 ** machine epsilon
 *************************************)

structure Ulps :
sig
    include RATIONAL
    val E : t
end
=
struct
    open NormalizedRational

    val Izero = IntInf.fromInt 0
    val Ione = IntInf.fromInt 1
    val Itwo = IntInf.fromInt 2

    val e = IntInf.pow (Itwo, 53)
    val E = one / (fromIntInf e) 


    (* print the errors in Shewchuk style *)
    (* as sums of machine epsilons *)

    fun approx2 p =
	let val {num=a, denom=b} = toFrac p
	    fun approx' (a, b, exp) = 
		let open IntInf
		in
		    if a = Izero then (a, a, exp)
		    else
			if a >= b then 
			    let val (q, r) = quotrem (a, b)
				val (r, s) = quotrem (r * e, b)
			    in
				if s = Izero then (q, r, exp)
				else (q, r + Ione, exp)
			    end
			else approx' (a * e, b, Int.+(exp, 1))
		end
	in
	    approx' (a, b, 0)
	end


    fun toString p = 
	let val (q, r, exp) = approx2 p
	    val qq = NumberTheory.approx IEEEReal.TO_POSINF q
	    val rr = NumberTheory.approx IEEEReal.TO_POSINF r
	    fun multEs 0 = ""
	      | multEs i = "* E " ^ (multEs (Int.-(i, 1)))
	in
	    "("^(Real.toString qq)^" + "^(Real.toString rr)^"*E) "^(multEs exp)
	end

 
(* Old code. May still be useful for debugging  *)

(*
    fun toString p = 
	let val (q, r, exp) = approx2 p
	    open IntInf
	in
	    (toString q)^"E"^(Int.toString exp)^" + "^
	    (toString r)^"E"^(Int.toString (Int.+(exp, 1)))
	end



    val N = 5
    fun approxN p =
	let val {num=a, denom=b} = toFrac p
	    fun approx' (a, b, exp, acc) =
		if (exp = N) orelse (a = Izero) then acc else
		    let open IntInf
		    in
			if a >= b then 
			    let val (q, r) = quotrem (a, b)
			    in
				if r = Izero then {coef=q, exp=exp}::acc
				else approx' (r * e, b, Int.+(exp, 1), {coef=q, exp=exp}::acc)
			    end
			else approx' (a * e, b, Int.+(exp, 1), acc)
		    end
	in
	    IntInfPoly.fromCoefExpList (approx' (a, b, 0, nil))
	end


    fun toString p =
	IntInfPoly.toString (approxN p)
*)

end
