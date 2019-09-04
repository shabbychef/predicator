(***************************************************************
 ** MinSphere.sml
 **
 ** Aleksandar Nanevski
 ** 
 ** 
 **
 ** Four implementations of 3d minSphere test.
 **
 ** The RationalMinSphere uses lazy rationals, i.e. 
 ** a structure where each operation is first performed in 
 ** interval arithmetic, and exact operations are
 ** called only if the intervals cannot compute the
 ** result precisely enough. 
 **
 ** WARNING : this code requires TO_NEGINF rounding mode.
 **
 **
 **
 ** The CompiledMinSphere uses Shewchuk-style error analysis
 ** and library for extended floats. It is produced automatically
 ** by the expression compiler. 
 **
 ** WARNING : this code requires TO_NEAREST rounding mode
 ** and currently CANNOT catch overflow/underflow (but only 
 ** because current version of SML-NJ doesn't provide means to 
 ** do that)
 **
 **
 ** The IntervalMinSphere uses interval analysis, but doesn't
 ** waste time on remembering the history of the computation.
 ** It's speed is twice slower than that of CompiledMinSphere.
 **
 ** WARNING : this code requires TO_NEGINF rounding mode.
 **
 **
 **
 ** The ExfloatMinSphere uses interval analysis for the first stage
 ** and ExtendedFloat exact arithmetic for the exact computations.
 ** It is exploiting the fact that if the fp computation fails, it
 ** goes immediately to the exact computation in CompiledMinSphere.
 ** Further improvement in the stage A speed could be done by using 
 ** Shewchuk approach for the first phase. However, in such a case
 ** the staging could not be exploited as much. So this is probably 
 ** the best implementation.
 **
 ** WARNING : this code requires TO_NEGINF rounding mode.
 ** It CANNOT catch underflows.
 ** 
 **
 **
 ** All four implementation are curried.
 **
 ** I did some preliminary runs for debugging 
 ** purposes. The second implementation is some 9-10 times
 ** faster than the first and some 1.5-2 times faster than the third.
 ** However, I didn't run the test on realistic data. Both the Rational
 ** and especially the Interval implementation have a chance of
 ** beating the Compiled one because they make (for now) better use of
 ** staging.
 **
 ** The fourth implementation, ExfloatMinSphere, is some 1.5 times
 ** faster than CompiledMinSphere, and it exploits staging, so it
 ** is probably the best (even though it could be further sped up 
 ** if we are ready to give up the staging benefits).
 *****************************************************************)


structure RationalMinSphere :>
sig
    type point = real * real * real
    val minSphere3 : point * point * point -> point -> int
end =
struct
    structure Q = RoundDownLazyRational
    type point = real * real * real
	
    fun minSphere3 ((ax, ay, az), (bx, by, bz), (cx, cy, cz)) =
	let open RoundDownLazyRational 
	    val [ax, ay, az, bx, by, bz, cx, cy, cz] =
		map fromReal [ax, ay, az, bx, by, bz, cx, cy, cz]
	    val (abx, aby, abz) = (bx - ax, by - ay, bz - az)
	    val (acx, acy, acz) = (cx - ax, cy - ay, cz - az)
		
	    val (ab, ac, bc) = (abx * abx + aby * aby + abz * abz,
				acx * acx + acy * acy + acz * acz,
				abx * acx + aby * acy + abz * acz)
	    val T = ab * ac
	    val detA = T - bc * bc
	    val detB = T - ac * bc
	    val detC = T - ab * bc
		
	    val ccx = ax * detA + abx * detB + acx * detC
	    val ccy = ay * detA + aby * detB + acy * detC
	    val ccz = az * detA + abz * detB + acz * detC
	in
	    fn (px, py, pz) =>
	    let val [px, py, pz] = map fromReal [px, py, pz]
		val apx = px - ax
		val apy = py - ay
		val apz = pz - az	 
		val s = apx * (ccx - detA * px) + apy * (ccy - detA * py) + apz * (ccz - detA * pz)
	    in
		sign s
	    end
	end
end



structure CompiledMinSphere :>
sig
    type point = real * real * real
    val minSphere3 : point * point * point -> point -> int
end =
struct
    structure R = Real
    structure Q = ExtendedFloat
    structure X = XFloat

    type point = real * real * real


    fun susp f = 
	let val x = ref (fn () => raise Domain)
	in 
	    x before 
	    x := (fn () => let val c = f() 
			   in 
			       c before
			       x := (fn () => c) 
			   end) 
	end
    

    val minSphere3 =
let val E = R.fromManExp{man=1.0, exp= ~53}
    val rm = IEEEReal.getRoundingMode()
    val _ = IEEEReal.setRoundingMode IEEEReal.TO_POSINF
val errA = (20.0 + 251.0*E) * E 
val errB = (6.0 + 92.0*E) * E 
val errC = (122.0 + 2559.0*E) * E * E 
val errCX = (2.0 + 7.0*E) * E 
    val _ = IEEEReal.setRoundingMode rm
in
fn ( (ax, ay, az), (bx, by, bz), (cx, cy, cz) ) =>
let val abx = R.-(bx, ax)
val aby = R.-(by, ay)
val abz = R.-(bz, az)
val acx = R.-(cx, ax)
val acy = R.-(cy, ay)
val acz = R.-(cz, az)
val ab = R.*(abx, abx)
val ab2 = R.*(aby, aby)
val ab3 = R.*(abz, abz)
val ab4 = R.+(ab2, ab3)
val ab5 = R.+(ab, ab4)
val ac = R.*(acx, acx)
val ac2 = R.*(acy, acy)
val ac3 = R.*(acz, acz)
val ac4 = R.+(ac2, ac3)
val ac5 = R.+(ac, ac4)
val bc = R.*(abx, acx)
val bc2 = R.*(aby, acy)
val bc3 = R.*(abz, acz)
val bc4 = R.+(bc2, bc3)
val bcP4 = R.+(R.abs(bc2), R.abs(bc3))
val bc5 = R.+(bc, bc4)
val bcP5 = R.+(R.abs(bc), bcP4)
val T = R.*(ab5, ac5)
val detA = R.*(bc5, bc5)
val detAP = R.*(bcP5, bcP5)
val detA2 = R.-(T, detA)
val detAP2 = R.+(T, detAP)
val detB = R.*(ac5, bc5)
val detBP = R.*(ac5, bcP5)
val detB2 = R.-(T, detB)
val detBP2 = R.+(T, detBP)
val detC = R.*(ab5, bc5)
val detCP = R.*(ab5, bcP5)
val detC2 = R.-(T, detC)
val detCP2 = R.+(T, detCP)
val ccx = R.*(ax, detA2)
val ccxP = R.*(R.abs(ax), detAP2)
val ccx2 = R.*(abx, detB2)
val ccxP2 = R.*(R.abs(abx), detBP2)
val ccx3 = R.*(acx, detC2)
val ccxP3 = R.*(R.abs(acx), detCP2)
val ccx4 = R.+(ccx2, ccx3)
val ccxP4 = R.+(ccxP2, ccxP3)
val ccx5 = R.+(ccx, ccx4)
val ccxP5 = R.+(ccxP, ccxP4)
val ccy = R.*(ay, detA2)
val ccyP = R.*(R.abs(ay), detAP2)
val ccy2 = R.*(aby, detB2)
val ccyP2 = R.*(R.abs(aby), detBP2)
val ccy3 = R.*(acy, detC2)
val ccyP3 = R.*(R.abs(acy), detCP2)
val ccy4 = R.+(ccy2, ccy3)
val ccyP4 = R.+(ccyP2, ccyP3)
val ccy5 = R.+(ccy, ccy4)
val ccyP5 = R.+(ccyP, ccyP4)
val ccz = R.*(az, detA2)
val cczP = R.*(R.abs(az), detAP2)
val ccz2 = R.*(abz, detB2)
val cczP2 = R.*(R.abs(abz), detBP2)
val ccz3 = R.*(acz, detC2)
val cczP3 = R.*(R.abs(acz), detCP2)
val ccz4 = R.+(ccz2, ccz3)
val cczP4 = R.+(cczP2, cczP3)
val ccz5 = R.+(ccz, ccz4)
val cczP5 = R.+(cczP, cczP4)
val suspB2 = susp (fn () => 
let 
val abB = Q.sq(abx)
val abB2 = Q.sq(aby)
val abB3 = Q.sq(abz)
val abB4 = Q.+(abB2, abB3)
val abB5 = Q.add2(abB4, abB)
val acB = Q.sq(acx)
val acB2 = Q.sq(acy)
val acB3 = Q.sq(acz)
val acB4 = Q.+(acB2, acB3)
val acB5 = Q.add2(acB4, acB)
val bcB = Q.prod(abx, acx)
val bcB2 = Q.prod(aby, acy)
val bcB3 = Q.prod(abz, acz)
val bcB4 = Q.+(bcB2, bcB3)
val bcB5 = Q.add2(bcB4, bcB)
val TB = X.*(abB5, acB5)
val detAB = X.*(bcB5, bcB5)
val detAB2 = X.-(TB, detAB)
val detBB = X.*(acB5, bcB5)
val detBB2 = X.-(TB, detBB)
val detCB = X.*(abB5, bcB5)
val detCB2 = X.-(TB, detCB)
val ccxB = X.scale(ax, detAB2)
val ccxB2 = X.scale(abx, detBB2)
val ccxB3 = X.scale(acx, detCB2)
val ccxB4 = X.+(ccxB2, ccxB3)
val ccxB5 = X.+(ccxB, ccxB4)
val ccyB = X.scale(ay, detAB2)
val ccyB2 = X.scale(aby, detBB2)
val ccyB3 = X.scale(acy, detCB2)
val ccyB4 = X.+(ccyB2, ccyB3)
val ccyB5 = X.+(ccyB, ccyB4)
val cczB = X.scale(az, detAB2)
val cczB2 = X.scale(abz, detBB2)
val cczB3 = X.scale(acz, detCB2)
val cczB4 = X.+(cczB2, cczB3)
val cczB5 = X.+(cczB, cczB4)

in
((abB5, acB5, bcB5, detBB2, detCB2), (ccxB5, ccyB5, cczB5, detAB2))
end)

val suspC2 = susp (fn () => 
let 
val abxC = #err (Q.toResErr(Q.diff(bx, ax)))
val abyC = #err (Q.toResErr(Q.diff(by, ay)))
val abzC = #err (Q.toResErr(Q.diff(bz, az)))
val acxC = #err (Q.toResErr(Q.diff(cx, ax)))
val acyC = #err (Q.toResErr(Q.diff(cy, ay)))
val aczC = #err (Q.toResErr(Q.diff(cz, az)))
val abC = R.*(2.0, R.*(abx, abxC))
val abC2 = R.*(2.0, R.*(aby, abyC))
val abC3 = R.*(2.0, R.*(abz, abzC))
val abC4 = R.+(abC2, abC3)
val abC5 = R.+(abC, abC4)
val acC = R.*(2.0, R.*(acx, acxC))
val acC2 = R.*(2.0, R.*(acy, acyC))
val acC3 = R.*(2.0, R.*(acz, aczC))
val acC4 = R.+(acC2, acC3)
val acC5 = R.+(acC, acC4)
val bcC = R.+(R.*(abx, acxC), R.*(abxC, acx))
val bcC2 = R.+(R.*(aby, acyC), R.*(abyC, acy))
val bcC3 = R.+(R.*(abz, aczC), R.*(abzC, acz))
val bcC4 = R.+(bcC2, bcC3)
val bcC5 = R.+(bcC, bcC4)
val TC = R.+(R.*(ab5, acC5), R.*(abC5, ac5))
val detAC = R.*(2.0, R.*(bc5, bcC5))
val detAC2 = R.-(TC, detAC)
val detBC = R.+(R.*(ac5, bcC5), R.*(acC5, bc5))
val detBC2 = R.-(TC, detBC)
val detCC = R.+(R.*(ab5, bcC5), R.*(abC5, bc5))
val detCC2 = R.-(TC, detCC)
val ccxC = R.*(ax, detAC2)
val ccxC2 = R.+(R.*(abx, detBC2), R.*(abxC, detB2))
val ccxC3 = R.+(R.*(acx, detCC2), R.*(acxC, detC2))
val ccxC4 = R.+(ccxC2, ccxC3)
val ccxC5 = R.+(ccxC, ccxC4)
val ccyC = R.*(ay, detAC2)
val ccyC2 = R.+(R.*(aby, detBC2), R.*(abyC, detB2))
val ccyC3 = R.+(R.*(acy, detCC2), R.*(acyC, detC2))
val ccyC4 = R.+(ccyC2, ccyC3)
val ccyC5 = R.+(ccyC, ccyC4)
val cczC = R.*(az, detAC2)
val cczC2 = R.+(R.*(abz, detBC2), R.*(abzC, detB2))
val cczC3 = R.+(R.*(acz, detCC2), R.*(aczC, detC2))
val cczC4 = R.+(cczC2, cczC3)
val cczC5 = R.+(cczC, cczC4)

in
((abxC, abyC, abzC, acxC, acyC, aczC), (ccxC5, ccyC5, cczC5, detAC2))
end)

val suspD2 = susp (fn () => 
let 
val ((abB5, acB5, bcB5, detBB2, detCB2), _) = !suspB2()

val ((abxC, abyC, abzC, acxC, acyC, aczC), _) = !suspC2()

val abD = Q.+(Q.double(Q.prod(abx, abxC)), Q.sq(abxC))
val abD2 = Q.+(Q.double(Q.prod(aby, abyC)), Q.sq(abyC))
val abD3 = Q.+(Q.double(Q.prod(abz, abzC)), Q.sq(abzC))
val abD4 = X.+(abD2, abD3)
val abD5 = X.+(abD, abD4)
val acD = Q.+(Q.double(Q.prod(acx, acxC)), Q.sq(acxC))
val acD2 = Q.+(Q.double(Q.prod(acy, acyC)), Q.sq(acyC))
val acD3 = Q.+(Q.double(Q.prod(acz, aczC)), Q.sq(aczC))
val acD4 = X.+(acD2, acD3)
val acD5 = X.+(acD, acD4)
val bcD = Q.add2(Q.+(Q.prod(abx, acxC), Q.prod(abxC, acx)), Q.prod(abxC, acxC))
val bcD2 = Q.add2(Q.+(Q.prod(aby, acyC), Q.prod(abyC, acy)), Q.prod(abyC, acyC))
val bcD3 = Q.add2(Q.+(Q.prod(abz, aczC), Q.prod(abzC, acz)), Q.prod(abzC, aczC))
val bcD4 = X.+(bcD2, bcD3)
val bcD5 = X.+(bcD, bcD4)
val TD = X.+(X.+(X.*(abB5, acD5), X.*(abD5, acB5)), X.*(abD5, acD5))
val detAD = X.+(X.double(X.*(bcB5, bcD5)), X.*(bcD5, bcD5))
val detAD2 = X.-(TD, detAD)
val detBD = X.+(X.+(X.*(acB5, bcD5), X.*(acD5, bcB5)), X.*(acD5, bcD5))
val detBD2 = X.-(TD, detBD)
val detCD = X.+(X.+(X.*(abB5, bcD5), X.*(abD5, bcB5)), X.*(abD5, bcD5))
val detCD2 = X.-(TD, detCD)
val ccxD = X.scale(ax, detAD2)
val ccxD2 = X.+(X.+(X.scale(abx, detBD2), X.scale(abxC, detBB2)), X.scale(abxC, detBD2))
val ccxD3 = X.+(X.+(X.scale(acx, detCD2), X.scale(acxC, detCB2)), X.scale(acxC, detCD2))
val ccxD4 = X.+(ccxD2, ccxD3)
val ccxD5 = X.+(ccxD, ccxD4)
val ccyD = X.scale(ay, detAD2)
val ccyD2 = X.+(X.+(X.scale(aby, detBD2), X.scale(abyC, detBB2)), X.scale(abyC, detBD2))
val ccyD3 = X.+(X.+(X.scale(acy, detCD2), X.scale(acyC, detCB2)), X.scale(acyC, detCD2))
val ccyD4 = X.+(ccyD2, ccyD3)
val ccyD5 = X.+(ccyD, ccyD4)
val cczD = X.scale(az, detAD2)
val cczD2 = X.+(X.+(X.scale(abz, detBD2), X.scale(abzC, detBB2)), X.scale(abzC, detBD2))
val cczD3 = X.+(X.+(X.scale(acz, detCD2), X.scale(aczC, detCB2)), X.scale(aczC, detCD2))
val cczD4 = X.+(cczD2, cczD3)
val cczD5 = X.+(cczD, cczD4)

in
(ccxD5, ccyD5, cczD5, detAD2)
end)


in
fn ( px, py, pz ) =>
let val apx = R.-(px, ax)
val apy = R.-(py, ay)
val apz = R.-(pz, az)
val d = R.*(detA2, px)
val dP = R.*(detAP2, R.abs(px))
val d2 = R.-(ccx5, d)
val dP2 = R.+(ccxP5, dP)
val d3 = R.*(apx, d2)
val dP3 = R.*(R.abs(apx), dP2)
val d4 = R.*(detA2, py)
val dP4 = R.*(detAP2, R.abs(py))
val d5 = R.-(ccy5, d4)
val dP5 = R.+(ccyP5, dP4)
val d6 = R.*(apy, d5)
val dP6 = R.*(R.abs(apy), dP5)
val d7 = R.*(detA2, pz)
val dP7 = R.*(detAP2, R.abs(pz))
val d8 = R.-(ccz5, d7)
val dP8 = R.+(cczP5, dP7)
val d9 = R.*(apz, d8)
val dP9 = R.*(R.abs(apz), dP8)
val d10 = R.+(d6, d9)
val dP10 = R.+(dP6, dP9)
val d11 = R.+(d3, d10)
val dP11 = R.+(dP3, dP10)
val yAE = R.*(errA, dP11)

in

if d11 > yAE then 1
else if R.~(d11) > yAE then ~1
     else 
let val (_, (ccxB5, ccyB5, cczB5, detAB2)) = !suspB2()

val dB = X.scale(px, detAB2)
val dB2 = X.-(ccxB5, dB)
val dB3 = X.scale(apx, dB2)
val dB4 = X.scale(py, detAB2)
val dB5 = X.-(ccyB5, dB4)
val dB6 = X.scale(apy, dB5)
val dB7 = X.scale(pz, detAB2)
val dB8 = X.-(cczB5, dB7)
val dB9 = X.scale(apz, dB8)
val dB10 = X.+(dB6, dB9)
val dB11 = X.+(dB3, dB10)
val yBX = X.approx(dB11)
val yBE = R.*(errB, dP11)

in
if yBX > yBE then 1
else if R.~(yBX) > yBE then ~1
     else 
let val (_, (ccxC5, ccyC5, cczC5, detAC2)) = !suspC2()

val apxC = #err (Q.toResErr(Q.diff(px, ax)))
val apyC = #err (Q.toResErr(Q.diff(py, ay)))
val apzC = #err (Q.toResErr(Q.diff(pz, az)))
val dC = R.*(detAC2, px)
val dC2 = R.-(ccxC5, dC)
val dC3 = R.+(R.*(apx, dC2), R.*(apxC, d2))
val dC4 = R.*(detAC2, py)
val dC5 = R.-(ccyC5, dC4)
val dC6 = R.+(R.*(apy, dC5), R.*(apyC, d5))
val dC7 = R.*(detAC2, pz)
val dC8 = R.-(cczC5, dC7)
val dC9 = R.+(R.*(apz, dC8), R.*(apzC, d8))
val dC10 = R.+(dC6, dC9)
val dC11 = R.+(dC3, dC10)
val yCX = R.+(yBX, dC11)
val yCE = R.+(R.*(errCX, yBX), R.*(errC, dP11))

in
if yCX > yCE then 1
else if R.~(yCX) > yCE then ~1
     else 
let val (ccxD5, ccyD5, cczD5, detAD2) = !suspD2()

val dD = X.scale(px, detAD2)
val dD2 = X.-(ccxD5, dD)
val dD3 = X.+(X.+(X.scale(apx, dD2), X.scale(apxC, dB2)), X.scale(apxC, dD2))
val dD4 = X.scale(py, detAD2)
val dD5 = X.-(ccyD5, dD4)
val dD6 = X.+(X.+(X.scale(apy, dD5), X.scale(apyC, dB5)), X.scale(apyC, dD5))
val dD7 = X.scale(pz, detAD2)
val dD8 = X.-(cczD5, dD7)
val dD9 = X.+(X.+(X.scale(apz, dD8), X.scale(apzC, dB8)), X.scale(apzC, dD8))
val dD10 = X.+(dD6, dD9)
val dD11 = X.+(dD3, dD10)

in
X.sign(X.+(dB11, dD11))
end

end

end

end
end
end (* main let *)
    
    
end


structure IntervalMinSphere :>
sig
    type point = real * real * real
    val minSphere3 : point * point * point -> point -> int
end =
struct
    structure Q = NormalizedRational
    structure I = RoundDownFloatInterval
	
    type point = real * real * real

    fun minSphere3 ((ax', ay', az'), (bx', by', bz'), (cx', cy', cz')) =
	let open I
	    val [ax, ay, az, bx, by, bz, cx, cy, cz] =
		map fromNumber [ax', ay', az', bx', by', bz', cx', cy', cz']
	    val (abx, aby, abz) = (bx - ax, by - ay, bz - az)
	    val (acx, acy, acz) = (cx - ax, cy - ay, cz - az)
		
	    val (ab, ac, bc) = (abx * abx + aby * aby + abz * abz,
				acx * acx + acy * acy + acz * acz,
				abx * acx + aby * acy + abz * acz)
	    val T = ab * ac
	    val detA = T - bc * bc
	    val detB = T - ac * bc
	    val detC = T - ab * bc
		
	    val ccx = ax * detA + abx * detB + acx * detC
	    val ccy = ay * detA + aby * detB + acy * detC
	    val ccz = az * detA + abz * detB + acz * detC
		
	    val data = ref (detA, ccx, ccy, ccz)
		
	    val exactB = (fn _ => 
	        let open Q
		    val [ax, ay, az, bx, by, bz, cx, cy, cz] =
			  map fromReal [ax', ay', az', bx', by', bz', cx', cy', cz']
			  
		    val (abx, aby, abz) = (bx - ax, by - ay, bz - az)
		    val (acx, acy, acz) = (cx - ax, cy - ay, cz - az)
			  
		    val (ab, ac, bc) = (abx * abx + aby * aby + abz * abz,
					acx * acx + acy * acy + acz * acz,
					abx * acx + aby * acy + abz * acz)
		    val T = ab * ac
		    val detA = T - bc * bc
		    val detB = T - ac * bc
		    val detC = T - ab * bc
			  
		    val ccx = ax * detA + abx * detB + acx * detC
		    val ccy = ay * detA + aby * detB + acy * detC
		    val ccz = az * detA + abz * detB + acz * detC
		in
			  data := (I.fromBounds (intervalApprox detA), 
				   I.fromBounds (intervalApprox ccx),
				   I.fromBounds (intervalApprox ccy), 
				   I.fromBounds (intervalApprox ccz));
			  (detA, ccx, ccy, ccz)
		end)
	in
	    fn (px', py', pz') =>
	    let open I
		val (detA, ccx, ccy, ccz) = !data
		val [ax, ay, az, px, py, pz] = map fromNumber [ax', ay', az', px', py', pz']
		val apx = px - ax
		val apy = py - ay
		val apz = pz - az	 
		val s = apx * (ccx - detA * px) + apy * (ccy - detA * py) + apz * (ccz - detA * pz)
	    in
		if s > zero then 1
		else if s < zero then ~1 
		     else
			 let open Q
			     val [ax, ay, az, px, py, pz] = map fromReal [ax', ay', az', px', py', pz']
			     val (detA, ccx, ccy, ccz) = exactB()
			     val apx = px - ax
			     val apy = py - ay
			     val apz = pz - az	 
			     val s = apx * (ccx - detA * px) + apy * (ccy - detA * py) + apz * (ccz - detA * pz)
			 in
			     sign s
			 end
	    end
	end
end







structure ExfloatMinSphere :>
sig
    type point = real * real * real
    val minSphere3 : point * point * point -> point -> int
end =
struct

    structure I = RoundDownFloatInterval
    structure QX = ExtendedFloat
    structure X = XFloat

    type point = real * real * real

    val eps2 = Real.fromManExp {man=1.0, exp= ~52}

    fun minSphere3 ((ax', ay', az'), (bx', by', bz'), (cx', cy', cz')) =
	let open I
	    val [ax, ay, az, bx, by, bz, cx, cy, cz] =
		map fromNumber [ax', ay', az', bx', by', bz', cx', cy', cz']
	    val (abx, aby, abz) = (bx - ax, by - ay, bz - az)
	    val (acx, acy, acz) = (cx - ax, cy - ay, cz - az)
		
	    val (ab, ac, bc) = (abx * abx + aby * aby + abz * abz,
				acx * acx + acy * acy + acz * acz,
				abx * acx + aby * acy + abz * acz)
	    val T = ab * ac
	    val detA = T - bc * bc
	    val detB = T - ac * bc
	    val detC = T - ab * bc
		
	    val ccx = ax * detA + abx * detB + acx * detC
	    val ccy = ay * detA + aby * detB + acy * detC
	    val ccz = az * detA + abz * detB + acz * detC
		
	    val data = ref (detA, ccx, ccy, ccz)
		
	    val exactB = (fn _ => 
	        let val _ = IEEEReal.setRoundingMode (IEEEReal.TO_NEAREST)
		    val (abx, aby, abz) = (QX.diff(bx', ax'), QX.diff(by', ay'), QX.diff(bz', az'))
		    val (acx, acy, acz) = (QX.diff(cx', ax'), QX.diff(cy', ay'), QX.diff(cz', az'))
			  
		    open X
		    val (ab, ac, bc) = (QX.sq2 abx + QX.sq2 aby + QX.sq2 abz,
					QX.sq2 acx + QX.sq2 acy + QX.sq2 acz,
					QX.*(abx,acx) + QX.*(aby, acy) + QX.*(abz, acz))
		    val T = ab * ac
		    val detA = T - bc * bc
		    val detB = T - ac * bc
		    val detC = T - ab * bc
			  
		    val ccx = scale(ax', detA) + QX.scale2(abx, detB) + QX.scale2(acx, detC)
		    val ccy = scale(ay', detA) + QX.scale2(aby, detB) + QX.scale2(acy, detC)
		    val ccz = scale(az', detA) + QX.scale2(abz, detB) + QX.scale2(acz, detC)
		    open Real
		    val detA' = abs (X.approx detA)
		    val errd = detA' * eps2
		    val (ccx', ccy', ccz') = (abs (X.approx ccx), abs (X.approx ccy), abs (X.approx ccz))
		    val (errx, erry, errz) = (ccx' * eps2, ccy' * eps2, ccz' * eps2)
		in
		    ((detA', errd, ccx', errx, ccy', erry, ccz', errz), (detA, ccx, ccy, ccz))
		end)
	in
	    fn (px', py', pz') =>
	    let open I
		val (detA, ccx, ccy, ccz) = !data
		val [ax, ay, az, px, py, pz] = map fromNumber [ax', ay', az', px', py', pz']
		val apx = px - ax
		val apy = py - ay
		val apz = pz - az	 
		val s = apx * (ccx - detA * px) + apy * (ccy - detA * py) + apz * (ccz - detA * pz)
	    in
		if s > zero then 1
		else if s < zero then ~1 
		     else
			 let val ((detA', errd, ccx', errx, ccy', erry, ccz', errz), 
				  (detA, ccx, ccy, ccz)) = exactB()
			     val apx = QX.diff(px', ax')
			     val apy = QX.diff(py', ay')
			     val apz = QX.diff(pz', az')
			     open X
			     val s = QX.scale2(apx, ccx - scale(px', detA)) + 
				 QX.scale2(apy, ccy - scale(py', detA)) + 
				 QX.scale2(apz, ccz - scale(pz', detA))
			 in
			     IEEEReal.setRoundingMode (IEEEReal.TO_NEGINF);
			     data := (I.fromMidErr {mid=detA', err=errd},
				      I.fromMidErr {mid=ccx', err=errx},
				      I.fromMidErr {mid=ccy', err=erry},
				      I.fromMidErr {mid=ccz', err=errz});
			     sign s
			 end
	    end
	end
end

