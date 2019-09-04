(***************************************
 ** ExactPredicates.sml
 ** sml
 **
 ** Aleksandar Nanevski
 **
 ** A structure for automatically 
 ** generated geometric predicates.
 **
 ** TODO:
 ** - can these functions be compiled
 ** in some other way except cut&paste?
 ***************************************)


structure ExactPredicates =
struct

    structure R = Real
    structure Q = ExtendedFloat
    structure X = XFloat


    val orient2 = 
	"fn [ ax, ay, bx, by, cx, cy ] => " ^
	"   let val acx = ax - cx " ^ 
        "       val acy = ay - cy " ^

        "       val d = acy * ( cx - bx ) + acx * ( by - cy ) " ^
        "   end"


    (* staged orient2 testing version *)
    val orient2' =
	"fn [ ax, ay, bx, by ] => " ^
        "   let val abx = ax - bx " ^
        "       val aby = ay - by " ^
        "       fn [ cx, cy ] =>  " ^
        "          let val d = aby * ( cx - bx ) + abx * ( by - cy ) " ^
        "          end " ^
        "   end"


    val orient3 =
	"fn [ ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz ] => " ^
	"   let val bcx = bx - cx " ^
	"       val acx = ax - cx " ^
        "       val bcy = by - cy " ^
        "       val acy = ay - cy " ^
        "       val bcz = bz - cz " ^
        "       val acz = az - cz " ^
        "       val xMinor = ( bcy * acz ) - ( acy * bcz ) " ^
        "       val yMinor = ( bcz * acx ) - ( acz * bcx ) " ^
        "       val zMinor = ( bcx * acy ) - ( acx * bcy ) " ^
       
	"       val dcx = dx - cx " ^
        "       val dcy = dy - cy " ^
        "       val dcz = dz - cz " ^
        "       val d = dcx * xMinor + dcy * yMinor + dcz * zMinor " ^
        "   end"



    val orient3' =
	"fn [ ax, ay, az, bx, by, bz, cx, cy, cz ] => " ^
	"   let val bcx = bx - cx " ^
	"       val acx = ax - cx " ^
        "       val bcy = by - cy " ^
        "       val acy = ay - cy " ^
        "       val bcz = bz - cz " ^
        "       val acz = az - cz " ^
        "       val xMinor = ( bcy * acz ) - ( acy * bcz ) " ^
        "       val yMinor = ( bcz * acx ) - ( acz * bcx ) " ^
        "       val zMinor = ( bcx * acy ) - ( acx * bcy ) " ^
        "       fn [ dx, dy, dz ] => " ^
	"          let val dcx = dx - cx " ^
        "              val dcy = dy - cy " ^
        "              val dcz = dz - cz " ^
        "              val d = dcx * xMinor + dcy * yMinor + dcz * zMinor " ^
        "          end" ^
        "   end"


    (* orient3 if the plane normal is given *)
    val normalOrient3 = 
	"fn [ xMinor, yMinor, zMinor, cx, cy, cz, dx, dy, dz ] => " ^
	"   let val dcx = dx - cx " ^ 
	"       val dcy = dy - cy " ^
        "       val dcz = dz - cz " ^
        "       val d = dcx * xMinor + dcy * yMinor * dcz * zMinor " ^
	"   end "




    val insphere2 =
	"fn [ ax, ay, bx, by, cx, cy, dx, dy ] => " ^
	"   let val bcx = bx - cx " ^
        "       val acx = ax - cx " ^
        "       val bcy = by - cy " ^
        "       val acy = ay - cy " ^
	"       val acSquare = ( sq acx ) + ( sq acy ) " ^ 
	"       val bcSquare = ( sq bcx ) + ( sq bcy ) " ^
	"       val xMinor = ( bcy * acSquare ) - ( acy * bcSquare ) " ^
	"       val yMinor = ( bcSquare * acx ) - ( acSquare * bcx ) " ^
	"       val zMinor = ( bcx * acy ) - ( acx * bcy ) " ^ 

        "       val dcx = dx - cx " ^
        "       val dcy = dy - cy " ^
	"       val dcSquare = ( sq dcx ) + ( sq dcy ) " ^
	"       val d = dcx * xMinor + dcy * yMinor + dcSquare * zMinor " ^
        "   end "



    val insphere2 =
	"fn [ ax, ay, bx, by, cx, cy, dx, dy ] => " ^
	"   let val adx = ax - dx " ^
        "       val bdx = bx - dx " ^
        "       val cdx = cx - dx " ^
        "       val ady = ay - dy " ^
        "       val bdy = by - dy " ^
        "       val cdy = cy - dy " ^

	"       val aminor = ( bdx * cdy ) - ( cdx * bdy ) " ^ 
	"       val bminor = ( cdx * ady ) - ( adx * cdy ) " ^ 
	"       val cminor = ( adx * bdy ) - ( bdx * ady ) " ^ 

	"       val alift = ( sq adx ) + ( sq ady ) " ^
	"       val blift = ( sq bdx ) + ( sq bdy ) " ^
	"       val clift = ( sq cdx ) + ( sq cdy ) " ^

	"       val d = alift * aminor + ( blift * bminor + clift * cminor ) " ^
        "   end "



	



    val insphere3 = 
	"fn [ ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz, ex, ey, ez ] => " ^
	"   let val adx = ax - dx " ^
	"       val bdx = bx - dx " ^ 
	"       val cdx = cx - dx " ^
	"       val ady = ay - dy " ^
	"       val bdy = by - dy " ^
        "       val cdy = cy - dy " ^
	"       val adz = az - dz " ^
	"       val bdz = bz - dz " ^
	"       val cdz = cz - dz " ^
	"       val adSquare = ( sq adx ) + ( sq ady ) + ( sq adz ) " ^
	"       val bdSquare = ( sq bdx ) + ( sq bdy ) + ( sq bdz ) " ^
	"       val cdSquare = ( sq cdx ) + ( sq cdy ) + ( sq cdz ) " ^
	"       val abxy = ( adx * bdy ) - ( bdx * ady ) " ^
	"       val abyz = ( ady * bdz ) - ( bdy * adz ) " ^
	"       val abxz = ( adx * bdz ) - ( bdx * adz ) " ^
	"       val bcxy = ( bdx * cdy ) - ( cdx * bdy ) " ^
	"       val bcyz = ( bdy * cdz ) - ( cdy * bdz ) " ^
	"       val bcxz = ( bdx * cdz ) - ( cdx * bdz ) " ^
	"       val caxy = ( cdx * ady ) - ( adx * cdy ) " ^
	"       val cayz = ( cdy * adz ) - ( ady * cdz ) " ^
	"       val caxz = ( cdx * adz ) - ( adx * cdz ) " ^
	"       val xMinor = bcyz * adSquare + cayz * bdSquare + abyz * cdSquare " ^
	"       val yMinor = bcxz * adSquare + caxz * bdSquare + abxz * cdSquare " ^
	"       val zMinor = bcxy * adSquare + caxy * bdSquare + abxy * cdSquare " ^
	"       val uMinor = adx * bcyz + bdx * cayz + cdx * abyz " ^
	
        "       val edx = ex - dx " ^
	"       val edy = ey - dy " ^
	"       val edz = ez - dz " ^
	"       val edSquare = ( sq edx ) + ( sq edy ) + ( sq edz ) " ^
	"       val d = ( edx * xMinor - edy * yMinor ) + ( edz * zMinor - edSquare * uMinor ) " ^
	"end"





    (* staged version of insphere3 *)
    val insphere3' = 
	"fn [ ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz ] => " ^
	"   let val adx = ax - dx " ^
	"       val bdx = bx - dx " ^ 
	"       val cdx = cx - dx " ^
	"       val ady = ay - dy " ^
	"       val bdy = by - dy " ^
        "       val cdy = cy - dy " ^
	"       val adz = az - dz " ^
	"       val bdz = bz - dz " ^
	"       val cdz = cz - dz " ^
	"       val adSquare = ( sq adx ) + ( sq ady ) + ( sq adz ) " ^
	"       val bdSquare = ( sq bdx ) + ( sq bdy ) + ( sq bdz ) " ^
	"       val cdSquare = ( sq cdx ) + ( sq cdy ) + ( sq cdz ) " ^
	"       val abxy = ( adx * bdy ) - ( bdx * ady ) " ^
	"       val abyz = ( ady * bdz ) - ( bdy * adz ) " ^
	"       val abxz = ( adx * bdz ) - ( bdx * adz ) " ^
	"       val bcxy = ( bdx * cdy ) - ( cdx * bdy ) " ^
	"       val bcyz = ( bdy * cdz ) - ( cdy * bdz ) " ^
	"       val bcxz = ( bdx * cdz ) - ( cdx * bdz ) " ^
	"       val caxy = ( cdx * ady ) - ( adx * cdy ) " ^
	"       val cayz = ( cdy * adz ) - ( ady * cdz ) " ^
	"       val caxz = ( cdx * adz ) - ( adx * cdz ) " ^
	"       val xMinor = bcyz * adSquare + cayz * bdSquare + abyz * cdSquare " ^
	"       val yMinor = bcxz * adSquare + caxz * bdSquare + abxz * cdSquare " ^
	"       val zMinor = bcxy * adSquare + caxy * bdSquare + abxy * cdSquare " ^
	"       val uMinor = adx * bcyz + bdx * cayz + cdx * abyz " ^
	
	"       fn [ ex, ey, ez ] => " ^ 
        "       let val edx = ex - dx " ^
	"           val edy = ey - dy " ^
	"           val edz = ez - dz " ^
	"           val edSquare = ( sq edx ) + ( sq edy ) + ( sq edz ) " ^
	"           val d = ( edx * xMinor - edy * yMinor ) + ( edz * zMinor - edSquare * uMinor ) " ^
        "       end" ^
	"   end"









    (* insphere3 developed along the fourth column, rather than the row *)
    val insphere3'' = 
	"fn [ ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz, ex, ey, ez ] => " ^
	"   let val aex = ax - ex " ^
	"       val bex = bx - ex " ^ 
	"       val cex = cx - ex " ^
	"       val dex = dx - ex " ^
	"       val aey = ay - ey " ^
	"       val bey = by - ey " ^ 
	"       val cey = cy - ey " ^
	"       val dey = dy - ey " ^
	"       val aez = az - ez " ^
	"       val bez = bz - ez " ^ 
	"       val cez = cz - ez " ^
	"       val dez = dz - ez " ^
	"       val aeSquare = ( sq aex ) + ( sq aey ) + ( sq aez ) " ^
	"       val beSquare = ( sq bex ) + ( sq bey ) + ( sq bez ) " ^
	"       val ceSquare = ( sq cex ) + ( sq cey ) + ( sq cez ) " ^
	"       val deSquare = ( sq dex ) + ( sq dey ) + ( sq dez ) " ^
	
	"       val abxy = aex * bey - aey * bex " ^
	"       val bcxy = bex * cey - bey * cex " ^
	"       val cdxy = cex * dey - cey * dex " ^
	"       val daxy = dex * aey - dey * aex " ^
	"       val acxy = aex * cey - aey * cex " ^
	"       val bdxy = bex * dey - bey * dex " ^

	"       val aMinor = bez * cdxy - cez * bdxy + dez * bcxy " ^
	"       val bMinor = aez * cdxy + cez * daxy + dez * acxy " ^
	"       val cMinor = aez * bdxy + bez * daxy + dez * abxy " ^
	"       val dMinor = aez * bcxy - bez * acxy + cez * abxy " ^
	
	"       val aeSq = ( sq aex ) + ( sq aey ) + ( sq aez ) " ^
	"       val beSq = ( sq bex ) + ( sq bey ) + ( sq bez ) " ^
	"       val ceSq = ( sq cex ) + ( sq cey ) + ( sq cez ) " ^
	"       val deSq = ( sq dex ) + ( sq dey ) + ( sq dez ) " ^

	"       val d = aeSq * aMinor - beSq * bMinor + ceSq * cMinor - deSq * dMinor " ^
	"   end"
	






    (* min-sphere test *)
    val minsphere3 = 
	"fn [ ax, ay, az, bx, by, bz, cx, cy, cz, px, py, pz ] => " ^
	"   let val abx = bx - ax " ^
	"       val aby = by - ay " ^
	"       val abz = bz - az " ^
	"       val acx = cx - ax " ^
        "       val acy = cy - ay " ^
	"       val acz = cz - az " ^
	
        "       val ab = sq abx + sq aby + sq abz " ^
	"       val ac = sq acx + sq acy + sq acz " ^
	"       val bc = abx * acx + aby * acy + abz * acz " ^
        
	"       val T = ab * ac " ^
	"       val detA = T - sq bc" ^
	"       val detB = T - ac * bc " ^
	"       val detC = T - ab * bc " ^
	
	"       val ccx = ax * detA + abx * detB + acx * detC " ^
	"       val ccy = ay * detA + aby * detB + acy * detC " ^
	"       val ccz = az * detA + abz * detB + acz * detC " ^
	
	"       val apx = px - ax " ^
	"       val apy = py - ay " ^
	"       val apz = pz - az " ^

	"       val d = apx * ( ccx - detA * px ) + apy * ( ccy - detA * py ) + apz * ( ccz - detA * pz ) " ^
        "   end "



    (* staged version of minimal-sphere test *)
    val minsphere3' = 
	"fn [ ax, ay, az, bx, by, bz, cx, cy, cz ] => " ^
	"   let val abx = bx - ax " ^
	"       val aby = by - ay " ^
	"       val abz = bz - az " ^
	"       val acx = cx - ax " ^
        "       val acy = cy - ay " ^
	"       val acz = cz - az " ^

        "       val ab = sq abx + sq aby + sq abz " ^
	"       val ac = sq acx + sq acy + sq acz " ^
	"       val bc = abx * acx + aby * acy + abz * acz " ^
        
	"       val T = ab * ac " ^
	"       val detA = T - sq bc" ^
	"       val detB = T - ac * bc " ^
	"       val detC = T - ab * bc " ^
	
	"       val ccx = ax * detA + abx * detB + acx * detC " ^
	"       val ccy = ay * detA + aby * detB + acy * detC " ^
	"       val ccz = az * detA + abz * detB + acz * detC " ^

	"       fn [ px, py, pz ] => " ^ 
	"       let val apx = px - ax " ^
	"           val apy = py - ay " ^
	"           val apz = pz - az " ^

	"           val d = apx * ( ccx - detA * px ) + apy * ( ccy - detA * py ) + apz * ( ccz - detA * pz ) " ^
	"       end " ^
        "   end "



    (* staged dummy *)
    val dummy = 
	"fn [ a, b ] => " ^
	"   let val ab = a - b" ^
	"       val ab2 = sq ab" ^

	"       fn [ c ] => " ^ 
	"       let val d = c * ab2" ^
	"       end " ^
        "   end "





(********************************************
 ** helper function for creating suspensions
 ********************************************)
    
    fun susp f = 
	let val x = ref (fn () => raise Domain)
	in 
	    x := (fn () => let val c = f() 
			   in 
			       c before
			       x := (fn () => c) 
			   end);
	    fn () => (!x)()
	end


(**********************************************************************
 **  the following code is produced automatically 
 **********************************************************************)

    val smlOrient2 = 
let val E = R.fromManExp{man=1.0, exp= ~53}
    val rm = IEEEReal.getRoundingMode()
    val _ = IEEEReal.setRoundingMode IEEEReal.TO_POSINF
val errA = (3.0 + 13.0*E) * E 
val errB = (2.0 + 12.0*E) * E 
val errC = (7.0 + 42.0*E) * E * E 
val errCX = (2.0 + 7.0*E) * E 
    val _ = IEEEReal.setRoundingMode rm
in
fn [ ax, ay, bx, by, cx, cy ] =>
let val acx = R.-(ax, cx)
val acy = R.-(ay, cy)
val d = R.-(cx, bx)
val d2 = R.*(acy, d)
val d3 = R.-(by, cy)
val d4 = R.*(acx, d3)
val d5 = R.+(d2, d4)
val dP5 = R.+(R.abs(d2), R.abs(d4))
val yAE = R.*(errA, dP5)

in

if d5 > yAE then 1
else if R.~(d5) > yAE then ~1
     else 
let 
val dB2 = Q.prod(acy, d)
val dB4 = Q.prod(acx, d3)
val dB5 = Q.+(dB2, dB4)
val yBX = X.approx(dB5)
val yBE = R.*(errB, dP5)

in
if yBX > yBE then 1
else if R.~(yBX) > yBE then ~1
     else 
let 
val acxC = #err (Q.toResErr(Q.diff(ax, cx)))
val acyC = #err (Q.toResErr(Q.diff(ay, cy)))
val dC = #err (Q.toResErr(Q.diff(cx, bx)))
val dC2 = R.+(R.*(acy, dC), R.*(acyC, d))
val dC3 = #err (Q.toResErr(Q.diff(by, cy)))
val dC4 = R.+(R.*(acx, dC3), R.*(acxC, d3))
val dC5 = R.+(dC2, dC4)
val yCX = R.+(yBX, dC5)
val yCE = R.+(R.*(errCX, yBX), R.*(errC, dP5))

in
if yCX > yCE then 1
else if R.~(yCX) > yCE then ~1
     else 
let 
val dD2 = Q.add2(Q.+(Q.prod(acy, dC), Q.prod(acyC, d)), Q.prod(acyC, dC))
val dD4 = Q.add2(Q.+(Q.prod(acx, dC3), Q.prod(acxC, d3)), Q.prod(acxC, dC3))
val dD5 = X.+(dD2, dD4)

in
X.sign(X.+(dB5, dD5))
end

end

end

end
end (* main let *)


    val smlStagedOrient2 =
let val E = R.fromManExp{man=1.0, exp= ~53}
    val rm = IEEEReal.getRoundingMode()
    val _ = IEEEReal.setRoundingMode IEEEReal.TO_POSINF
val errA = (3.0 + 13.0*E) * E 
val errB = (2.0 + 12.0*E) * E 
val errC = (7.0 + 42.0*E) * E * E 
val errCX = (2.0 + 7.0*E) * E 
    val _ = IEEEReal.setRoundingMode rm
in
fn [ ax, ay, bx, by ] =>
let val abx = R.-(ax, bx)
val aby = R.-(ay, by)

val suspC2 = susp (fn () => 
let 
val abxC = #err (Q.toResErr(Q.diff(ax, bx)))
val abyC = #err (Q.toResErr(Q.diff(ay, by)))

in
(abxC, abyC)
end)



in
fn [ cx, cy ] =>
let val d = R.-(cx, bx)
val d2 = R.*(aby, d)
val d3 = R.-(by, cy)
val d4 = R.*(abx, d3)
val d5 = R.+(d2, d4)
val dP5 = R.+(R.abs(d2), R.abs(d4))
val yAE = R.*(errA, dP5)

in

if d5 > yAE then 1
else if R.~(d5) > yAE then ~1
     else 
let 
val dB2 = Q.prod(aby, d)
val dB4 = Q.prod(abx, d3)
val dB5 = Q.+(dB2, dB4)
val yBX = X.approx(dB5)
val yBE = R.*(errB, dP5)

in
if yBX > yBE then 1
else if R.~(yBX) > yBE then ~1
     else 
let val (abxC, abyC) = suspC2()

val dC = #err (Q.toResErr(Q.diff(cx, bx)))
val dC2 = R.+(R.*(aby, dC), R.*(abyC, d))
val dC3 = #err (Q.toResErr(Q.diff(by, cy)))
val dC4 = R.+(R.*(abx, dC3), R.*(abxC, d3))
val dC5 = R.+(dC2, dC4)
val yCX = R.+(yBX, dC5)
val yCE = R.+(R.*(errCX, yBX), R.*(errC, dP5))

in
if yCX > yCE then 1
else if R.~(yCX) > yCE then ~1
     else 
let 
val dD2 = Q.add2(Q.+(Q.prod(aby, dC), Q.prod(abyC, d)), Q.prod(abyC, dC))
val dD4 = Q.add2(Q.+(Q.prod(abx, dC3), Q.prod(abxC, d3)), Q.prod(abxC, dC3))
val dD5 = X.+(dD2, dD4)

in
X.sign(X.+(dB5, dD5))
end

end

end

end
end
end (* main let *)



    val smlOrient3 =
let val E = R.fromManExp{man=1.0, exp= ~53}
    val rm = IEEEReal.getRoundingMode()
    val _ = IEEEReal.setRoundingMode IEEEReal.TO_POSINF
val errA = (7.0 + 43.0*E) * E 
val errB = (3.0 + 26.0*E) * E 
val errC = (23.0 + 235.0*E) * E * E 
val errCX = (2.0 + 7.0*E) * E 
    val _ = IEEEReal.setRoundingMode rm
in
fn [ ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz ] =>
let val bcx = R.-(bx, cx)
val acx = R.-(ax, cx)
val bcy = R.-(by, cy)
val acy = R.-(ay, cy)
val bcz = R.-(bz, cz)
val acz = R.-(az, cz)
val xMinor = R.*(bcy, acz)
val xMinor2 = R.*(acy, bcz)
val xMinor3 = R.-(xMinor, xMinor2)
val xMinorP3 = R.+(R.abs(xMinor), R.abs(xMinor2))
val yMinor = R.*(bcz, acx)
val yMinor2 = R.*(acz, bcx)
val yMinor3 = R.-(yMinor, yMinor2)
val yMinorP3 = R.+(R.abs(yMinor), R.abs(yMinor2))
val zMinor = R.*(bcx, acy)
val zMinor2 = R.*(acx, bcy)
val zMinor3 = R.-(zMinor, zMinor2)
val zMinorP3 = R.+(R.abs(zMinor), R.abs(zMinor2))
val dcx = R.-(dx, cx)
val dcy = R.-(dy, cy)
val dcz = R.-(dz, cz)
val d = R.*(dcx, xMinor3)
val dP = R.*(R.abs(dcx), xMinorP3)
val d2 = R.*(dcy, yMinor3)
val dP2 = R.*(R.abs(dcy), yMinorP3)
val d3 = R.*(dcz, zMinor3)
val dP3 = R.*(R.abs(dcz), zMinorP3)
val d4 = R.+(d2, d3)
val dP4 = R.+(dP2, dP3)
val d5 = R.+(d, d4)
val dP5 = R.+(dP, dP4)
val yAE = R.*(errA, dP5)

in

if d5 > yAE then 1
else if R.~(d5) > yAE then ~1
     else 
let 
val xMinorB = Q.prod(bcy, acz)
val xMinorB2 = Q.prod(acy, bcz)
val xMinorB3 = Q.-(xMinorB, xMinorB2)
val yMinorB = Q.prod(bcz, acx)
val yMinorB2 = Q.prod(acz, bcx)
val yMinorB3 = Q.-(yMinorB, yMinorB2)
val zMinorB = Q.prod(bcx, acy)
val zMinorB2 = Q.prod(acx, bcy)
val zMinorB3 = Q.-(zMinorB, zMinorB2)
val dB = X.scale(dcx, xMinorB3)
val dB2 = X.scale(dcy, yMinorB3)
val dB3 = X.scale(dcz, zMinorB3)
val dB4 = X.+(dB2, dB3)
val dB5 = X.+(dB, dB4)
val yBX = X.approx(dB5)
val yBE = R.*(errB, dP5)

in
if yBX > yBE then 1
else if R.~(yBX) > yBE then ~1
     else 
let 
val bcxC = #err (Q.toResErr(Q.diff(bx, cx)))
val acxC = #err (Q.toResErr(Q.diff(ax, cx)))
val bcyC = #err (Q.toResErr(Q.diff(by, cy)))
val acyC = #err (Q.toResErr(Q.diff(ay, cy)))
val bczC = #err (Q.toResErr(Q.diff(bz, cz)))
val aczC = #err (Q.toResErr(Q.diff(az, cz)))
val xMinorC = R.+(R.*(bcy, aczC), R.*(bcyC, acz))
val xMinorC2 = R.+(R.*(acy, bczC), R.*(acyC, bcz))
val xMinorC3 = R.-(xMinorC, xMinorC2)
val yMinorC = R.+(R.*(bcz, acxC), R.*(bczC, acx))
val yMinorC2 = R.+(R.*(acz, bcxC), R.*(aczC, bcx))
val yMinorC3 = R.-(yMinorC, yMinorC2)
val zMinorC = R.+(R.*(bcx, acyC), R.*(bcxC, acy))
val zMinorC2 = R.+(R.*(acx, bcyC), R.*(acxC, bcy))
val zMinorC3 = R.-(zMinorC, zMinorC2)
val dcxC = #err (Q.toResErr(Q.diff(dx, cx)))
val dcyC = #err (Q.toResErr(Q.diff(dy, cy)))
val dczC = #err (Q.toResErr(Q.diff(dz, cz)))
val dC = R.+(R.*(dcx, xMinorC3), R.*(dcxC, xMinor3))
val dC2 = R.+(R.*(dcy, yMinorC3), R.*(dcyC, yMinor3))
val dC3 = R.+(R.*(dcz, zMinorC3), R.*(dczC, zMinor3))
val dC4 = R.+(dC2, dC3)
val dC5 = R.+(dC, dC4)
val yCX = R.+(yBX, dC5)
val yCE = R.+(R.*(errCX, yBX), R.*(errC, dP5))

in
if yCX > yCE then 1
else if R.~(yCX) > yCE then ~1
     else 
let 
val xMinorD = Q.add2(Q.+(Q.prod(bcy, aczC), Q.prod(bcyC, acz)), Q.prod(bcyC, aczC))
val xMinorD2 = Q.add2(Q.+(Q.prod(acy, bczC), Q.prod(acyC, bcz)), Q.prod(acyC, bczC))
val xMinorD3 = X.-(xMinorD, xMinorD2)
val yMinorD = Q.add2(Q.+(Q.prod(bcz, acxC), Q.prod(bczC, acx)), Q.prod(bczC, acxC))
val yMinorD2 = Q.add2(Q.+(Q.prod(acz, bcxC), Q.prod(aczC, bcx)), Q.prod(aczC, bcxC))
val yMinorD3 = X.-(yMinorD, yMinorD2)
val zMinorD = Q.add2(Q.+(Q.prod(bcx, acyC), Q.prod(bcxC, acy)), Q.prod(bcxC, acyC))
val zMinorD2 = Q.add2(Q.+(Q.prod(acx, bcyC), Q.prod(acxC, bcy)), Q.prod(acxC, bcyC))
val zMinorD3 = X.-(zMinorD, zMinorD2)
val dD = X.+(X.+(X.scale(dcx, xMinorD3), X.scale(dcxC, xMinorB3)), X.scale(dcxC, xMinorD3))
val dD2 = X.+(X.+(X.scale(dcy, yMinorD3), X.scale(dcyC, yMinorB3)), X.scale(dcyC, yMinorD3))
val dD3 = X.+(X.+(X.scale(dcz, zMinorD3), X.scale(dczC, zMinorB3)), X.scale(dczC, zMinorD3))
val dD4 = X.+(dD2, dD3)
val dD5 = X.+(dD, dD4)

in
X.sign(X.+(dB5, dD5))
end

end

end

end
end (* main let *)




    val smlStagedOrient3 =

let val E = R.fromManExp{man=1.0, exp= ~53}
    val rm = IEEEReal.getRoundingMode()
    val _ = IEEEReal.setRoundingMode IEEEReal.TO_POSINF
val errA = (7.0 + 43.0*E) * E 
val errB = (3.0 + 26.0*E) * E 
val errC = (23.0 + 235.0*E) * E * E 
val errCX = (2.0 + 7.0*E) * E 
    val _ = IEEEReal.setRoundingMode rm
in
fn [ ax, ay, az, bx, by, bz, cx, cy, cz ] =>
let val bcx = R.-(bx, cx)
val acx = R.-(ax, cx)
val bcy = R.-(by, cy)
val acy = R.-(ay, cy)
val bcz = R.-(bz, cz)
val acz = R.-(az, cz)
val xMinor = R.*(bcy, acz)
val xMinor2 = R.*(acy, bcz)
val xMinor3 = R.-(xMinor, xMinor2)
val xMinorP3 = R.+(R.abs(xMinor), R.abs(xMinor2))
val yMinor = R.*(bcz, acx)
val yMinor2 = R.*(acz, bcx)
val yMinor3 = R.-(yMinor, yMinor2)
val yMinorP3 = R.+(R.abs(yMinor), R.abs(yMinor2))
val zMinor = R.*(bcx, acy)
val zMinor2 = R.*(acx, bcy)
val zMinor3 = R.-(zMinor, zMinor2)
val zMinorP3 = R.+(R.abs(zMinor), R.abs(zMinor2))
val suspB2 = susp (fn () => 
let 
val xMinorB = Q.prod(bcy, acz)
val xMinorB2 = Q.prod(acy, bcz)
val xMinorB3 = Q.-(xMinorB, xMinorB2)
val yMinorB = Q.prod(bcz, acx)
val yMinorB2 = Q.prod(acz, bcx)
val yMinorB3 = Q.-(yMinorB, yMinorB2)
val zMinorB = Q.prod(bcx, acy)
val zMinorB2 = Q.prod(acx, bcy)
val zMinorB3 = Q.-(zMinorB, zMinorB2)

in
(xMinorB3, yMinorB3, zMinorB3)
end)

val suspC2 = susp (fn () => 
let 
val bcxC = #err (Q.toResErr(Q.diff(bx, cx)))
val acxC = #err (Q.toResErr(Q.diff(ax, cx)))
val bcyC = #err (Q.toResErr(Q.diff(by, cy)))
val acyC = #err (Q.toResErr(Q.diff(ay, cy)))
val bczC = #err (Q.toResErr(Q.diff(bz, cz)))
val aczC = #err (Q.toResErr(Q.diff(az, cz)))
val xMinorC = R.+(R.*(bcy, aczC), R.*(bcyC, acz))
val xMinorC2 = R.+(R.*(acy, bczC), R.*(acyC, bcz))
val xMinorC3 = R.-(xMinorC, xMinorC2)
val yMinorC = R.+(R.*(bcz, acxC), R.*(bczC, acx))
val yMinorC2 = R.+(R.*(acz, bcxC), R.*(aczC, bcx))
val yMinorC3 = R.-(yMinorC, yMinorC2)
val zMinorC = R.+(R.*(bcx, acyC), R.*(bcxC, acy))
val zMinorC2 = R.+(R.*(acx, bcyC), R.*(acxC, bcy))
val zMinorC3 = R.-(zMinorC, zMinorC2)

in
((acxC, acyC, aczC, bcxC, bcyC, bczC), (xMinorC3, yMinorC3, zMinorC3))
end)

val suspD2 = susp (fn () => 
let 

val ((acxC, acyC, aczC, bcxC, bcyC, bczC), _) = suspC2()

val xMinorD = Q.add2(Q.+(Q.prod(bcy, aczC), Q.prod(bcyC, acz)), Q.prod(bcyC, aczC))
val xMinorD2 = Q.add2(Q.+(Q.prod(acy, bczC), Q.prod(acyC, bcz)), Q.prod(acyC, bczC))
val xMinorD3 = X.-(xMinorD, xMinorD2)
val yMinorD = Q.add2(Q.+(Q.prod(bcz, acxC), Q.prod(bczC, acx)), Q.prod(bczC, acxC))
val yMinorD2 = Q.add2(Q.+(Q.prod(acz, bcxC), Q.prod(aczC, bcx)), Q.prod(aczC, bcxC))
val yMinorD3 = X.-(yMinorD, yMinorD2)
val zMinorD = Q.add2(Q.+(Q.prod(bcx, acyC), Q.prod(bcxC, acy)), Q.prod(bcxC, acyC))
val zMinorD2 = Q.add2(Q.+(Q.prod(acx, bcyC), Q.prod(acxC, bcy)), Q.prod(acxC, bcyC))
val zMinorD3 = X.-(zMinorD, zMinorD2)

in
(xMinorD3, yMinorD3, zMinorD3)
end)


in
fn [ dx, dy, dz ] =>
let val dcx = R.-(dx, cx)
val dcy = R.-(dy, cy)
val dcz = R.-(dz, cz)
val d = R.*(dcx, xMinor3)
val dP = R.*(R.abs(dcx), xMinorP3)
val d2 = R.*(dcy, yMinor3)
val dP2 = R.*(R.abs(dcy), yMinorP3)
val d3 = R.*(dcz, zMinor3)
val dP3 = R.*(R.abs(dcz), zMinorP3)
val d4 = R.+(d2, d3)
val dP4 = R.+(dP2, dP3)
val d5 = R.+(d, d4)
val dP5 = R.+(dP, dP4)
val yAE = R.*(errA, dP5)

in

if d5 > yAE then 1
else if R.~(d5) > yAE then ~1
     else 
let val (xMinorB3, yMinorB3, zMinorB3) = suspB2()

val dB = X.scale(dcx, xMinorB3)
val dB2 = X.scale(dcy, yMinorB3)
val dB3 = X.scale(dcz, zMinorB3)
val dB4 = X.+(dB2, dB3)
val dB5 = X.+(dB, dB4)
val yBX = X.approx(dB5)
val yBE = R.*(errB, dP5)

in
if yBX > yBE then 1
else if R.~(yBX) > yBE then ~1
     else 
let val (_, (xMinorC3, yMinorC3, zMinorC3)) = suspC2()

val dcxC = #err (Q.toResErr(Q.diff(dx, cx)))
val dcyC = #err (Q.toResErr(Q.diff(dy, cy)))
val dczC = #err (Q.toResErr(Q.diff(dz, cz)))
val dC = R.+(R.*(dcx, xMinorC3), R.*(dcxC, xMinor3))
val dC2 = R.+(R.*(dcy, yMinorC3), R.*(dcyC, yMinor3))
val dC3 = R.+(R.*(dcz, zMinorC3), R.*(dczC, zMinor3))
val dC4 = R.+(dC2, dC3)
val dC5 = R.+(dC, dC4)
val yCX = R.+(yBX, dC5)
val yCE = R.+(R.*(errCX, yBX), R.*(errC, dP5))

in
if yCX > yCE then 1
else if R.~(yCX) > yCE then ~1
     else 
let val (xMinorD3, yMinorD3, zMinorD3) = suspD2()

val dD = X.+(X.+(X.scale(dcx, xMinorD3), X.scale(dcxC, xMinorB3)), X.scale(dcxC, xMinorD3))
val dD2 = X.+(X.+(X.scale(dcy, yMinorD3), X.scale(dcyC, yMinorB3)), X.scale(dcyC, yMinorD3))
val dD3 = X.+(X.+(X.scale(dcz, zMinorD3), X.scale(dczC, zMinorB3)), X.scale(dczC, zMinorD3))
val dD4 = X.+(dD2, dD3)
val dD5 = X.+(dD, dD4)

in
X.sign(X.+(dB5, dD5))
end

end

end

end
end
end (* main let *)




    val smlInSphere2 =
let val E = R.fromManExp{man=1.0, exp= ~53}
    val rm = IEEEReal.getRoundingMode()
    val _ = IEEEReal.setRoundingMode IEEEReal.TO_POSINF
val errA = (10.0 + 76.0*E) * E 
val errB = (4.0 + 39.0*E) * E 
val errC = (40.0 + 493.0*E) * E * E 
val errCX = (2.0 + 7.0*E) * E 
    val _ = IEEEReal.setRoundingMode rm
in
fn [ ax, ay, bx, by, cx, cy, dx, dy ] =>
let val adx = R.-(ax, dx)
val bdx = R.-(bx, dx)
val cdx = R.-(cx, dx)
val ady = R.-(ay, dy)
val bdy = R.-(by, dy)
val cdy = R.-(cy, dy)
val aminor = R.*(bdx, cdy)
val aminor2 = R.*(cdx, bdy)
val aminor3 = R.-(aminor, aminor2)
val aminorP3 = R.+(R.abs(aminor), R.abs(aminor2))
val bminor = R.*(cdx, ady)
val bminor2 = R.*(adx, cdy)
val bminor3 = R.-(bminor, bminor2)
val bminorP3 = R.+(R.abs(bminor), R.abs(bminor2))
val cminor = R.*(adx, bdy)
val cminor2 = R.*(bdx, ady)
val cminor3 = R.-(cminor, cminor2)
val cminorP3 = R.+(R.abs(cminor), R.abs(cminor2))
val alift = R.*(adx, adx)
val alift2 = R.*(ady, ady)
val alift3 = R.+(alift, alift2)
val blift = R.*(bdx, bdx)
val blift2 = R.*(bdy, bdy)
val blift3 = R.+(blift, blift2)
val clift = R.*(cdx, cdx)
val clift2 = R.*(cdy, cdy)
val clift3 = R.+(clift, clift2)
val d = R.*(alift3, aminor3)
val dP = R.*(alift3, aminorP3)
val d2 = R.*(blift3, bminor3)
val dP2 = R.*(blift3, bminorP3)
val d3 = R.*(clift3, cminor3)
val dP3 = R.*(clift3, cminorP3)
val d4 = R.+(d2, d3)
val dP4 = R.+(dP2, dP3)
val d5 = R.+(d, d4)
val dP5 = R.+(dP, dP4)
val yAE = R.*(errA, dP5)

in

if d5 > yAE then 1
else if R.~(d5) > yAE then ~1
     else 
let 
val aminorB = Q.prod(bdx, cdy)
val aminorB2 = Q.prod(cdx, bdy)
val aminorB3 = Q.-(aminorB, aminorB2)
val bminorB = Q.prod(cdx, ady)
val bminorB2 = Q.prod(adx, cdy)
val bminorB3 = Q.-(bminorB, bminorB2)
val cminorB = Q.prod(adx, bdy)
val cminorB2 = Q.prod(bdx, ady)
val cminorB3 = Q.-(cminorB, cminorB2)
val aliftB = Q.sq(adx)
val aliftB2 = Q.sq(ady)
val aliftB3 = Q.+(aliftB, aliftB2)
val bliftB = Q.sq(bdx)
val bliftB2 = Q.sq(bdy)
val bliftB3 = Q.+(bliftB, bliftB2)
val cliftB = Q.sq(cdx)
val cliftB2 = Q.sq(cdy)
val cliftB3 = Q.+(cliftB, cliftB2)
val dB = X.*(aliftB3, aminorB3)
val dB2 = X.*(bliftB3, bminorB3)
val dB3 = X.*(cliftB3, cminorB3)
val dB4 = X.+(dB2, dB3)
val dB5 = X.+(dB, dB4)
val yBX = X.approx(dB5)
val yBE = R.*(errB, dP5)

in
if yBX > yBE then 1
else if R.~(yBX) > yBE then ~1
     else 
let 
val adxC = #err (Q.toResErr(Q.diff(ax, dx)))
val bdxC = #err (Q.toResErr(Q.diff(bx, dx)))
val cdxC = #err (Q.toResErr(Q.diff(cx, dx)))
val adyC = #err (Q.toResErr(Q.diff(ay, dy)))
val bdyC = #err (Q.toResErr(Q.diff(by, dy)))
val cdyC = #err (Q.toResErr(Q.diff(cy, dy)))
val aminorC = R.+(R.*(bdx, cdyC), R.*(bdxC, cdy))
val aminorC2 = R.+(R.*(cdx, bdyC), R.*(cdxC, bdy))
val aminorC3 = R.-(aminorC, aminorC2)
val bminorC = R.+(R.*(cdx, adyC), R.*(cdxC, ady))
val bminorC2 = R.+(R.*(adx, cdyC), R.*(adxC, cdy))
val bminorC3 = R.-(bminorC, bminorC2)
val cminorC = R.+(R.*(adx, bdyC), R.*(adxC, bdy))
val cminorC2 = R.+(R.*(bdx, adyC), R.*(bdxC, ady))
val cminorC3 = R.-(cminorC, cminorC2)
val aliftC = R.*(2.0, R.*(adx, adxC))
val aliftC2 = R.*(2.0, R.*(ady, adyC))
val aliftC3 = R.+(aliftC, aliftC2)
val bliftC = R.*(2.0, R.*(bdx, bdxC))
val bliftC2 = R.*(2.0, R.*(bdy, bdyC))
val bliftC3 = R.+(bliftC, bliftC2)
val cliftC = R.*(2.0, R.*(cdx, cdxC))
val cliftC2 = R.*(2.0, R.*(cdy, cdyC))
val cliftC3 = R.+(cliftC, cliftC2)
val dC = R.+(R.*(alift3, aminorC3), R.*(aliftC3, aminor3))
val dC2 = R.+(R.*(blift3, bminorC3), R.*(bliftC3, bminor3))
val dC3 = R.+(R.*(clift3, cminorC3), R.*(cliftC3, cminor3))
val dC4 = R.+(dC2, dC3)
val dC5 = R.+(dC, dC4)
val yCX = R.+(yBX, dC5)
val yCE = R.+(R.*(errCX, yBX), R.*(errC, dP5))

in
if yCX > yCE then 1
else if R.~(yCX) > yCE then ~1
     else 
let 
val aminorD = Q.add2(Q.+(Q.prod(bdx, cdyC), Q.prod(bdxC, cdy)), Q.prod(bdxC, cdyC))
val aminorD2 = Q.add2(Q.+(Q.prod(cdx, bdyC), Q.prod(cdxC, bdy)), Q.prod(cdxC, bdyC))
val aminorD3 = X.-(aminorD, aminorD2)
val bminorD = Q.add2(Q.+(Q.prod(cdx, adyC), Q.prod(cdxC, ady)), Q.prod(cdxC, adyC))
val bminorD2 = Q.add2(Q.+(Q.prod(adx, cdyC), Q.prod(adxC, cdy)), Q.prod(adxC, cdyC))
val bminorD3 = X.-(bminorD, bminorD2)
val cminorD = Q.add2(Q.+(Q.prod(adx, bdyC), Q.prod(adxC, bdy)), Q.prod(adxC, bdyC))
val cminorD2 = Q.add2(Q.+(Q.prod(bdx, adyC), Q.prod(bdxC, ady)), Q.prod(bdxC, adyC))
val cminorD3 = X.-(cminorD, cminorD2)
val aliftD = Q.+(Q.double(Q.prod(adx, adxC)), Q.sq(adxC))
val aliftD2 = Q.+(Q.double(Q.prod(ady, adyC)), Q.sq(adyC))
val aliftD3 = X.+(aliftD, aliftD2)
val bliftD = Q.+(Q.double(Q.prod(bdx, bdxC)), Q.sq(bdxC))
val bliftD2 = Q.+(Q.double(Q.prod(bdy, bdyC)), Q.sq(bdyC))
val bliftD3 = X.+(bliftD, bliftD2)
val cliftD = Q.+(Q.double(Q.prod(cdx, cdxC)), Q.sq(cdxC))
val cliftD2 = Q.+(Q.double(Q.prod(cdy, cdyC)), Q.sq(cdyC))
val cliftD3 = X.+(cliftD, cliftD2)
val dD = X.+(X.+(X.*(aliftB3, aminorD3), X.*(aliftD3, aminorB3)), X.*(aliftD3, aminorD3))
val dD2 = X.+(X.+(X.*(bliftB3, bminorD3), X.*(bliftD3, bminorB3)), X.*(bliftD3, bminorD3))
val dD3 = X.+(X.+(X.*(cliftB3, cminorD3), X.*(cliftD3, cminorB3)), X.*(cliftD3, cminorD3))
val dD4 = X.+(dD2, dD3)
val dD5 = X.+(dD, dD4)

in
X.sign(X.+(dB5, dD5))
end

end

end

end
end (* main let *)



    local
	val E = R.fromManExp{man=1.0, exp= ~53}
	val rm = IEEEReal.getRoundingMode()
	val _ = IEEEReal.setRoundingMode IEEEReal.TO_POSINF
	val errA = (15.0 + 151.0*E) * E 
	val errB = (5.0 + 63.0*E) * E 
	val errC = (76.0 + 1269.0*E) * E * E 
	val errCX = (2.0 + 7.0*E) * E 
	val _ = IEEEReal.setRoundingMode rm
    in
	fun smlInSphere3 (args as [ ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz, ex, ey, ez ]) =
	    let val adx = R.-(ax, dx)
		val bdx = R.-(bx, dx)
		val cdx = R.-(cx, dx)
		val ady = R.-(ay, dy)
		val bdy = R.-(by, dy)
		val cdy = R.-(cy, dy)
		val adz = R.-(az, dz)
		val bdz = R.-(bz, dz)
		val cdz = R.-(cz, dz)
		val adSquare = R.*(adx, adx)
		val adSquare2 = R.*(ady, ady)
		val adSquare3 = R.*(adz, adz)
		val adSquare4 = R.+(adSquare2, adSquare3)
		val adSquare5 = R.+(adSquare, adSquare4)
		val bdSquare = R.*(bdx, bdx)
		val bdSquare2 = R.*(bdy, bdy)
		val bdSquare3 = R.*(bdz, bdz)
		val bdSquare4 = R.+(bdSquare2, bdSquare3)
		val bdSquare5 = R.+(bdSquare, bdSquare4)
		val cdSquare = R.*(cdx, cdx)
		val cdSquare2 = R.*(cdy, cdy)
		val cdSquare3 = R.*(cdz, cdz)
		val cdSquare4 = R.+(cdSquare2, cdSquare3)
		val cdSquare5 = R.+(cdSquare, cdSquare4)
		val abxy = R.*(adx, bdy)
		val abxy2 = R.*(bdx, ady)
		val abxy3 = R.-(abxy, abxy2)
		val abxyP3 = R.+(R.abs(abxy), R.abs(abxy2))
		val abyz = R.*(ady, bdz)
		val abyz2 = R.*(bdy, adz)
		val abyz3 = R.-(abyz, abyz2)
		val abyzP3 = R.+(R.abs(abyz), R.abs(abyz2))
		val abxz = R.*(adx, bdz)
		val abxz2 = R.*(bdx, adz)
		val abxz3 = R.-(abxz, abxz2)
		val abxzP3 = R.+(R.abs(abxz), R.abs(abxz2))
		val bcxy = R.*(bdx, cdy)
		val bcxy2 = R.*(cdx, bdy)
		val bcxy3 = R.-(bcxy, bcxy2)
		val bcxyP3 = R.+(R.abs(bcxy), R.abs(bcxy2))
		val bcyz = R.*(bdy, cdz)
		val bcyz2 = R.*(cdy, bdz)
		val bcyz3 = R.-(bcyz, bcyz2)
		val bcyzP3 = R.+(R.abs(bcyz), R.abs(bcyz2))
		val bcxz = R.*(bdx, cdz)
		val bcxz2 = R.*(cdx, bdz)
		val bcxz3 = R.-(bcxz, bcxz2)
		val bcxzP3 = R.+(R.abs(bcxz), R.abs(bcxz2))
		val caxy = R.*(cdx, ady)
		val caxy2 = R.*(adx, cdy)
		val caxy3 = R.-(caxy, caxy2)
		val caxyP3 = R.+(R.abs(caxy), R.abs(caxy2))
		val cayz = R.*(cdy, adz)
		val cayz2 = R.*(ady, cdz)
		val cayz3 = R.-(cayz, cayz2)
		val cayzP3 = R.+(R.abs(cayz), R.abs(cayz2))
		val caxz = R.*(cdx, adz)
		val caxz2 = R.*(adx, cdz)
		val caxz3 = R.-(caxz, caxz2)
		val caxzP3 = R.+(R.abs(caxz), R.abs(caxz2))
		val xMinor = R.*(bcyz3, adSquare5)
		val xMinorP = R.*(bcyzP3, adSquare5)
		val xMinor2 = R.*(cayz3, bdSquare5)
		val xMinorP2 = R.*(cayzP3, bdSquare5)
		val xMinor3 = R.*(abyz3, cdSquare5)
		val xMinorP3 = R.*(abyzP3, cdSquare5)
		val xMinor4 = R.+(xMinor2, xMinor3)
		val xMinorP4 = R.+(xMinorP2, xMinorP3)
		val xMinor5 = R.+(xMinor, xMinor4)
		val xMinorP5 = R.+(xMinorP, xMinorP4)
		val yMinor = R.*(bcxz3, adSquare5)
		val yMinorP = R.*(bcxzP3, adSquare5)
		val yMinor2 = R.*(caxz3, bdSquare5)
		val yMinorP2 = R.*(caxzP3, bdSquare5)
		val yMinor3 = R.*(abxz3, cdSquare5)
		val yMinorP3 = R.*(abxzP3, cdSquare5)
		val yMinor4 = R.+(yMinor2, yMinor3)
		val yMinorP4 = R.+(yMinorP2, yMinorP3)
		val yMinor5 = R.+(yMinor, yMinor4)
		val yMinorP5 = R.+(yMinorP, yMinorP4)
		val zMinor = R.*(bcxy3, adSquare5)
		val zMinorP = R.*(bcxyP3, adSquare5)
		val zMinor2 = R.*(caxy3, bdSquare5)
		val zMinorP2 = R.*(caxyP3, bdSquare5)
		val zMinor3 = R.*(abxy3, cdSquare5)
		val zMinorP3 = R.*(abxyP3, cdSquare5)
		val zMinor4 = R.+(zMinor2, zMinor3)
		val zMinorP4 = R.+(zMinorP2, zMinorP3)
		val zMinor5 = R.+(zMinor, zMinor4)
		val zMinorP5 = R.+(zMinorP, zMinorP4)
		val uMinor = R.*(adx, bcyz3)
		val uMinorP = R.*(R.abs(adx), bcyzP3)
		val uMinor2 = R.*(bdx, cayz3)
		val uMinorP2 = R.*(R.abs(bdx), cayzP3)
		val uMinor3 = R.*(cdx, abyz3)
		val uMinorP3 = R.*(R.abs(cdx), abyzP3)
		val uMinor4 = R.+(uMinor2, uMinor3)
		val uMinorP4 = R.+(uMinorP2, uMinorP3)
		val uMinor5 = R.+(uMinor, uMinor4)
		val uMinorP5 = R.+(uMinorP, uMinorP4)
		val edx = R.-(ex, dx)
		val edy = R.-(ey, dy)
		val edz = R.-(ez, dz)
		val edSquare = R.*(edx, edx)
		val edSquare2 = R.*(edy, edy)
		val edSquare3 = R.*(edz, edz)
		val edSquare4 = R.+(edSquare2, edSquare3)
		val edSquare5 = R.+(edSquare, edSquare4)
		val d = R.*(edx, xMinor5)
		val dP = R.*(R.abs(edx), xMinorP5)
		val d2 = R.*(edy, yMinor5)
		val dP2 = R.*(R.abs(edy), yMinorP5)
		val d3 = R.-(d, d2)
		val dP3 = R.+(dP, dP2)
		val d4 = R.*(edz, zMinor5)
		val dP4 = R.*(R.abs(edz), zMinorP5)
		val d5 = R.*(edSquare5, uMinor5)
		val dP5 = R.*(edSquare5, uMinorP5)
		val d6 = R.-(d4, d5)
		val dP6 = R.+(dP4, dP5)
		val d7 = R.+(d3, d6)
		val dP7 = R.+(dP3, dP6)
		val yAE = R.*(errA, dP7)
		    
	    in
		
		if d7 > yAE then 1
		else if R.~(d7) > yAE then ~1
		     else exactStage (args, 			
				      [adx, ady, adz, bdx, bdy, bdz, cdx, cdy, cdz,
				       edx, edy, edz, dP7, adSquare5, bcyz3, bdSquare5, cayz3, cdSquare5, abyz3,
				       bcxz3, caxz3, abxz3, bcxy3, caxy3, abxy3, xMinor5, yMinor5, zMinor5, uMinor5,
				       edSquare5]) 
	    end


	and exactStage ([ ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz, ex, ey, ez ], 
			[adx, ady, adz, bdx, bdy, bdz, cdx, cdy, cdz,
			 edx, edy, edz, dP7, adSquare5, bcyz3, bdSquare5, cayz3, cdSquare5, abyz3,
			 bcxz3, caxz3, abxz3, bcxy3, caxy3, abxy3, xMinor5, yMinor5, zMinor5, uMinor5,
			 edSquare5]
			 ) =
	    let val adSquareB = Q.sq(adx)
		val adSquareB2 = Q.sq(ady)
		val adSquareB3 = Q.sq(adz)
		val adSquareB4 = Q.+(adSquareB2, adSquareB3)
		val adSquareB5 = Q.add2(adSquareB4, adSquareB)
		val bdSquareB = Q.sq(bdx)
		val bdSquareB2 = Q.sq(bdy)
		val bdSquareB3 = Q.sq(bdz)
		val bdSquareB4 = Q.+(bdSquareB2, bdSquareB3)
		val bdSquareB5 = Q.add2(bdSquareB4, bdSquareB)
		val cdSquareB = Q.sq(cdx)
		val cdSquareB2 = Q.sq(cdy)
		val cdSquareB3 = Q.sq(cdz)
		val cdSquareB4 = Q.+(cdSquareB2, cdSquareB3)
		val cdSquareB5 = Q.add2(cdSquareB4, cdSquareB)
		val abxyB = Q.prod(adx, bdy)
		val abxyB2 = Q.prod(bdx, ady)
		val abxyB3 = Q.-(abxyB, abxyB2)
		val abyzB = Q.prod(ady, bdz)
		val abyzB2 = Q.prod(bdy, adz)
		val abyzB3 = Q.-(abyzB, abyzB2)
		val abxzB = Q.prod(adx, bdz)
		val abxzB2 = Q.prod(bdx, adz)
		val abxzB3 = Q.-(abxzB, abxzB2)
		val bcxyB = Q.prod(bdx, cdy)
		val bcxyB2 = Q.prod(cdx, bdy)
		val bcxyB3 = Q.-(bcxyB, bcxyB2)
		val bcyzB = Q.prod(bdy, cdz)
		val bcyzB2 = Q.prod(cdy, bdz)
		val bcyzB3 = Q.-(bcyzB, bcyzB2)
		val bcxzB = Q.prod(bdx, cdz)
		val bcxzB2 = Q.prod(cdx, bdz)
		val bcxzB3 = Q.-(bcxzB, bcxzB2)
		val caxyB = Q.prod(cdx, ady)
		val caxyB2 = Q.prod(adx, cdy)
		val caxyB3 = Q.-(caxyB, caxyB2)
		val cayzB = Q.prod(cdy, adz)
		val cayzB2 = Q.prod(ady, cdz)
		val cayzB3 = Q.-(cayzB, cayzB2)
		val caxzB = Q.prod(cdx, adz)
		val caxzB2 = Q.prod(adx, cdz)
		val caxzB3 = Q.-(caxzB, caxzB2)
		val xMinorB = X.*(bcyzB3, adSquareB5)
		val xMinorB2 = X.*(cayzB3, bdSquareB5)
		val xMinorB3 = X.*(abyzB3, cdSquareB5)
		val xMinorB4 = X.+(xMinorB2, xMinorB3)
		val xMinorB5 = X.+(xMinorB, xMinorB4)
		val yMinorB = X.*(bcxzB3, adSquareB5)
		val yMinorB2 = X.*(caxzB3, bdSquareB5)
		val yMinorB3 = X.*(abxzB3, cdSquareB5)
		val yMinorB4 = X.+(yMinorB2, yMinorB3)
		val yMinorB5 = X.+(yMinorB, yMinorB4)
		val zMinorB = X.*(bcxyB3, adSquareB5)
		val zMinorB2 = X.*(caxyB3, bdSquareB5)
		val zMinorB3 = X.*(abxyB3, cdSquareB5)
		val zMinorB4 = X.+(zMinorB2, zMinorB3)
		val zMinorB5 = X.+(zMinorB, zMinorB4)
		val uMinorB = X.scale(adx, bcyzB3)
		val uMinorB2 = X.scale(bdx, cayzB3)
		val uMinorB3 = X.scale(cdx, abyzB3)
		val uMinorB4 = X.+(uMinorB2, uMinorB3)
		val uMinorB5 = X.+(uMinorB, uMinorB4)
		val edSquareB = Q.sq(edx)
		val edSquareB2 = Q.sq(edy)
		val edSquareB3 = Q.sq(edz)
		val edSquareB4 = Q.+(edSquareB2, edSquareB3)
		val edSquareB5 = Q.add2(edSquareB4, edSquareB)
		val dB = X.scale(edx, xMinorB5)
		val dB2 = X.scale(edy, yMinorB5)
		val dB3 = X.-(dB, dB2)
		val dB4 = X.scale(edz, zMinorB5)
		val dB5 = X.*(edSquareB5, uMinorB5)
		val dB6 = X.-(dB4, dB5)
		val dB7 = X.+(dB3, dB6)
		val yBX = X.approx(dB7)
		val yBE = R.*(errB, dP7)
		    
	    in
		if yBX > yBE then 1
		else if R.~(yBX) > yBE then ~1
		     else 
			 let 
			     val adxC = #err (Q.toResErr(Q.diff(ax, dx)))
			     val bdxC = #err (Q.toResErr(Q.diff(bx, dx)))
			     val cdxC = #err (Q.toResErr(Q.diff(cx, dx)))
			     val adyC = #err (Q.toResErr(Q.diff(ay, dy)))
			     val bdyC = #err (Q.toResErr(Q.diff(by, dy)))
			     val cdyC = #err (Q.toResErr(Q.diff(cy, dy)))
			     val adzC = #err (Q.toResErr(Q.diff(az, dz)))
			     val bdzC = #err (Q.toResErr(Q.diff(bz, dz)))
			     val cdzC = #err (Q.toResErr(Q.diff(cz, dz)))
			     val adSquareC = R.*(2.0, R.*(adx, adxC))
			     val adSquareC2 = R.*(2.0, R.*(ady, adyC))
			     val adSquareC3 = R.*(2.0, R.*(adz, adzC))
			     val adSquareC4 = R.+(adSquareC2, adSquareC3)
			     val adSquareC5 = R.+(adSquareC, adSquareC4)
			     val bdSquareC = R.*(2.0, R.*(bdx, bdxC))
			     val bdSquareC2 = R.*(2.0, R.*(bdy, bdyC))
			     val bdSquareC3 = R.*(2.0, R.*(bdz, bdzC))
			     val bdSquareC4 = R.+(bdSquareC2, bdSquareC3)
			     val bdSquareC5 = R.+(bdSquareC, bdSquareC4)
			     val cdSquareC = R.*(2.0, R.*(cdx, cdxC))
			     val cdSquareC2 = R.*(2.0, R.*(cdy, cdyC))
			     val cdSquareC3 = R.*(2.0, R.*(cdz, cdzC))
			     val cdSquareC4 = R.+(cdSquareC2, cdSquareC3)
			     val cdSquareC5 = R.+(cdSquareC, cdSquareC4)
			     val abxyC = R.+(R.*(adx, bdyC), R.*(adxC, bdy))
			     val abxyC2 = R.+(R.*(bdx, adyC), R.*(bdxC, ady))
			     val abxyC3 = R.-(abxyC, abxyC2)
			     val abyzC = R.+(R.*(ady, bdzC), R.*(adyC, bdz))
			     val abyzC2 = R.+(R.*(bdy, adzC), R.*(bdyC, adz))
			     val abyzC3 = R.-(abyzC, abyzC2)
			     val abxzC = R.+(R.*(adx, bdzC), R.*(adxC, bdz))
			     val abxzC2 = R.+(R.*(bdx, adzC), R.*(bdxC, adz))
			     val abxzC3 = R.-(abxzC, abxzC2)
			     val bcxyC = R.+(R.*(bdx, cdyC), R.*(bdxC, cdy))
			     val bcxyC2 = R.+(R.*(cdx, bdyC), R.*(cdxC, bdy))
			     val bcxyC3 = R.-(bcxyC, bcxyC2)
			     val bcyzC = R.+(R.*(bdy, cdzC), R.*(bdyC, cdz))
			     val bcyzC2 = R.+(R.*(cdy, bdzC), R.*(cdyC, bdz))
			     val bcyzC3 = R.-(bcyzC, bcyzC2)
			     val bcxzC = R.+(R.*(bdx, cdzC), R.*(bdxC, cdz))
			     val bcxzC2 = R.+(R.*(cdx, bdzC), R.*(cdxC, bdz))
			     val bcxzC3 = R.-(bcxzC, bcxzC2)
			     val caxyC = R.+(R.*(cdx, adyC), R.*(cdxC, ady))
			     val caxyC2 = R.+(R.*(adx, cdyC), R.*(adxC, cdy))
			     val caxyC3 = R.-(caxyC, caxyC2)
			     val cayzC = R.+(R.*(cdy, adzC), R.*(cdyC, adz))
			     val cayzC2 = R.+(R.*(ady, cdzC), R.*(adyC, cdz))
			     val cayzC3 = R.-(cayzC, cayzC2)
			     val caxzC = R.+(R.*(cdx, adzC), R.*(cdxC, adz))
			     val caxzC2 = R.+(R.*(adx, cdzC), R.*(adxC, cdz))
			     val caxzC3 = R.-(caxzC, caxzC2)
			     val xMinorC = R.+(R.*(bcyz3, adSquareC5), R.*(bcyzC3, adSquare5))
			     val xMinorC2 = R.+(R.*(cayz3, bdSquareC5), R.*(cayzC3, bdSquare5))
			     val xMinorC3 = R.+(R.*(abyz3, cdSquareC5), R.*(abyzC3, cdSquare5))
			     val xMinorC4 = R.+(xMinorC2, xMinorC3)
			     val xMinorC5 = R.+(xMinorC, xMinorC4)
			     val yMinorC = R.+(R.*(bcxz3, adSquareC5), R.*(bcxzC3, adSquare5))
			     val yMinorC2 = R.+(R.*(caxz3, bdSquareC5), R.*(caxzC3, bdSquare5))
			     val yMinorC3 = R.+(R.*(abxz3, cdSquareC5), R.*(abxzC3, cdSquare5))
			     val yMinorC4 = R.+(yMinorC2, yMinorC3)
			     val yMinorC5 = R.+(yMinorC, yMinorC4)
			     val zMinorC = R.+(R.*(bcxy3, adSquareC5), R.*(bcxyC3, adSquare5))
			     val zMinorC2 = R.+(R.*(caxy3, bdSquareC5), R.*(caxyC3, bdSquare5))
			     val zMinorC3 = R.+(R.*(abxy3, cdSquareC5), R.*(abxyC3, cdSquare5))
			     val zMinorC4 = R.+(zMinorC2, zMinorC3)
			     val zMinorC5 = R.+(zMinorC, zMinorC4)
			     val uMinorC = R.+(R.*(adx, bcyzC3), R.*(adxC, bcyz3))
			     val uMinorC2 = R.+(R.*(bdx, cayzC3), R.*(bdxC, cayz3))
			     val uMinorC3 = R.+(R.*(cdx, abyzC3), R.*(cdxC, abyz3))
			     val uMinorC4 = R.+(uMinorC2, uMinorC3)
			     val uMinorC5 = R.+(uMinorC, uMinorC4)
			     val edxC = #err (Q.toResErr(Q.diff(ex, dx)))
			     val edyC = #err (Q.toResErr(Q.diff(ey, dy)))
			     val edzC = #err (Q.toResErr(Q.diff(ez, dz)))
			     val edSquareC = R.*(2.0, R.*(edx, edxC))
			     val edSquareC2 = R.*(2.0, R.*(edy, edyC))
			     val edSquareC3 = R.*(2.0, R.*(edz, edzC))
			     val edSquareC4 = R.+(edSquareC2, edSquareC3)
			     val edSquareC5 = R.+(edSquareC, edSquareC4)
			     val dC = R.+(R.*(edx, xMinorC5), R.*(edxC, xMinor5))
			     val dC2 = R.+(R.*(edy, yMinorC5), R.*(edyC, yMinor5))
			     val dC3 = R.-(dC, dC2)
			     val dC4 = R.+(R.*(edz, zMinorC5), R.*(edzC, zMinor5))
			     val dC5 = R.+(R.*(edSquare5, uMinorC5), R.*(edSquareC5, uMinor5))
			     val dC6 = R.-(dC4, dC5)
			     val dC7 = R.+(dC3, dC6)
			     val yCX = R.+(yBX, dC7)
			     val yCE = R.+(R.*(errCX, yBX), R.*(errC, dP7))
				 
			 in
			     if yCX > yCE then 1
			     else if R.~(yCX) > yCE then ~1
				  else 
				      let 
					  val adSquareD = Q.+(Q.double(Q.prod(adx, adxC)), Q.sq(adxC))
					  val adSquareD2 = Q.+(Q.double(Q.prod(ady, adyC)), Q.sq(adyC))
					  val adSquareD3 = Q.+(Q.double(Q.prod(adz, adzC)), Q.sq(adzC))
					  val adSquareD4 = X.+(adSquareD2, adSquareD3)
					  val adSquareD5 = X.+(adSquareD, adSquareD4)
					  val bdSquareD = Q.+(Q.double(Q.prod(bdx, bdxC)), Q.sq(bdxC))
					  val bdSquareD2 = Q.+(Q.double(Q.prod(bdy, bdyC)), Q.sq(bdyC))
					  val bdSquareD3 = Q.+(Q.double(Q.prod(bdz, bdzC)), Q.sq(bdzC))
					  val bdSquareD4 = X.+(bdSquareD2, bdSquareD3)
					  val bdSquareD5 = X.+(bdSquareD, bdSquareD4)
					  val cdSquareD = Q.+(Q.double(Q.prod(cdx, cdxC)), Q.sq(cdxC))
					  val cdSquareD2 = Q.+(Q.double(Q.prod(cdy, cdyC)), Q.sq(cdyC))
					  val cdSquareD3 = Q.+(Q.double(Q.prod(cdz, cdzC)), Q.sq(cdzC))
					  val cdSquareD4 = X.+(cdSquareD2, cdSquareD3)
					  val cdSquareD5 = X.+(cdSquareD, cdSquareD4)
					  val abxyD = Q.add2(Q.+(Q.prod(adx, bdyC), Q.prod(adxC, bdy)), Q.prod(adxC, bdyC))
					  val abxyD2 = Q.add2(Q.+(Q.prod(bdx, adyC), Q.prod(bdxC, ady)), Q.prod(bdxC, adyC))
					  val abxyD3 = X.-(abxyD, abxyD2)
					  val abyzD = Q.add2(Q.+(Q.prod(ady, bdzC), Q.prod(adyC, bdz)), Q.prod(adyC, bdzC))
					  val abyzD2 = Q.add2(Q.+(Q.prod(bdy, adzC), Q.prod(bdyC, adz)), Q.prod(bdyC, adzC))
					  val abyzD3 = X.-(abyzD, abyzD2)
					  val abxzD = Q.add2(Q.+(Q.prod(adx, bdzC), Q.prod(adxC, bdz)), Q.prod(adxC, bdzC))
					  val abxzD2 = Q.add2(Q.+(Q.prod(bdx, adzC), Q.prod(bdxC, adz)), Q.prod(bdxC, adzC))
					  val abxzD3 = X.-(abxzD, abxzD2)
					  val bcxyD = Q.add2(Q.+(Q.prod(bdx, cdyC), Q.prod(bdxC, cdy)), Q.prod(bdxC, cdyC))
					  val bcxyD2 = Q.add2(Q.+(Q.prod(cdx, bdyC), Q.prod(cdxC, bdy)), Q.prod(cdxC, bdyC))
					  val bcxyD3 = X.-(bcxyD, bcxyD2)
					  val bcyzD = Q.add2(Q.+(Q.prod(bdy, cdzC), Q.prod(bdyC, cdz)), Q.prod(bdyC, cdzC))
					  val bcyzD2 = Q.add2(Q.+(Q.prod(cdy, bdzC), Q.prod(cdyC, bdz)), Q.prod(cdyC, bdzC))
					  val bcyzD3 = X.-(bcyzD, bcyzD2)
					  val bcxzD = Q.add2(Q.+(Q.prod(bdx, cdzC), Q.prod(bdxC, cdz)), Q.prod(bdxC, cdzC))
					  val bcxzD2 = Q.add2(Q.+(Q.prod(cdx, bdzC), Q.prod(cdxC, bdz)), Q.prod(cdxC, bdzC))
					  val bcxzD3 = X.-(bcxzD, bcxzD2)
					  val caxyD = Q.add2(Q.+(Q.prod(cdx, adyC), Q.prod(cdxC, ady)), Q.prod(cdxC, adyC))
					  val caxyD2 = Q.add2(Q.+(Q.prod(adx, cdyC), Q.prod(adxC, cdy)), Q.prod(adxC, cdyC))
					  val caxyD3 = X.-(caxyD, caxyD2)
					  val cayzD = Q.add2(Q.+(Q.prod(cdy, adzC), Q.prod(cdyC, adz)), Q.prod(cdyC, adzC))
					  val cayzD2 = Q.add2(Q.+(Q.prod(ady, cdzC), Q.prod(adyC, cdz)), Q.prod(adyC, cdzC))
					  val cayzD3 = X.-(cayzD, cayzD2)
					  val caxzD = Q.add2(Q.+(Q.prod(cdx, adzC), Q.prod(cdxC, adz)), Q.prod(cdxC, adzC))
					  val caxzD2 = Q.add2(Q.+(Q.prod(adx, cdzC), Q.prod(adxC, cdz)), Q.prod(adxC, cdzC))
					  val caxzD3 = X.-(caxzD, caxzD2)
					  val xMinorD = X.+(X.+(X.*(bcyzB3, adSquareD5), X.*(bcyzD3, adSquareB5)), X.*(bcyzD3, adSquareD5))
					  val xMinorD2 = X.+(X.+(X.*(cayzB3, bdSquareD5), X.*(cayzD3, bdSquareB5)), X.*(cayzD3, bdSquareD5))
					  val xMinorD3 = X.+(X.+(X.*(abyzB3, cdSquareD5), X.*(abyzD3, cdSquareB5)), X.*(abyzD3, cdSquareD5))
					  val xMinorD4 = X.+(xMinorD2, xMinorD3)
					  val xMinorD5 = X.+(xMinorD, xMinorD4)
					  val yMinorD = X.+(X.+(X.*(bcxzB3, adSquareD5), X.*(bcxzD3, adSquareB5)), X.*(bcxzD3, adSquareD5))
					  val yMinorD2 = X.+(X.+(X.*(caxzB3, bdSquareD5), X.*(caxzD3, bdSquareB5)), X.*(caxzD3, bdSquareD5))
					  val yMinorD3 = X.+(X.+(X.*(abxzB3, cdSquareD5), X.*(abxzD3, cdSquareB5)), X.*(abxzD3, cdSquareD5))
					  val yMinorD4 = X.+(yMinorD2, yMinorD3)
					  val yMinorD5 = X.+(yMinorD, yMinorD4)
					  val zMinorD = X.+(X.+(X.*(bcxyB3, adSquareD5), X.*(bcxyD3, adSquareB5)), X.*(bcxyD3, adSquareD5))
					  val zMinorD2 = X.+(X.+(X.*(caxyB3, bdSquareD5), X.*(caxyD3, bdSquareB5)), X.*(caxyD3, bdSquareD5))
					  val zMinorD3 = X.+(X.+(X.*(abxyB3, cdSquareD5), X.*(abxyD3, cdSquareB5)), X.*(abxyD3, cdSquareD5))
					  val zMinorD4 = X.+(zMinorD2, zMinorD3)
					  val zMinorD5 = X.+(zMinorD, zMinorD4)
					  val uMinorD = X.+(X.+(X.scale(adx, bcyzD3), X.scale(adxC, bcyzB3)), X.scale(adxC, bcyzD3))
					  val uMinorD2 = X.+(X.+(X.scale(bdx, cayzD3), X.scale(bdxC, cayzB3)), X.scale(bdxC, cayzD3))
					  val uMinorD3 = X.+(X.+(X.scale(cdx, abyzD3), X.scale(cdxC, abyzB3)), X.scale(cdxC, abyzD3))
					  val uMinorD4 = X.+(uMinorD2, uMinorD3)
					  val uMinorD5 = X.+(uMinorD, uMinorD4)
					  val edSquareD = Q.+(Q.double(Q.prod(edx, edxC)), Q.sq(edxC))
					  val edSquareD2 = Q.+(Q.double(Q.prod(edy, edyC)), Q.sq(edyC))
					  val edSquareD3 = Q.+(Q.double(Q.prod(edz, edzC)), Q.sq(edzC))
					  val edSquareD4 = X.+(edSquareD2, edSquareD3)
					  val edSquareD5 = X.+(edSquareD, edSquareD4)
					  val dD = X.+(X.+(X.scale(edx, xMinorD5), X.scale(edxC, xMinorB5)), X.scale(edxC, xMinorD5))
					  val dD2 = X.+(X.+(X.scale(edy, yMinorD5), X.scale(edyC, yMinorB5)), X.scale(edyC, yMinorD5))
					  val dD3 = X.-(dD, dD2)
					  val dD4 = X.+(X.+(X.scale(edz, zMinorD5), X.scale(edzC, zMinorB5)), X.scale(edzC, zMinorD5))
					  val dD5 = X.+(X.+(X.*(edSquareB5, uMinorD5), X.*(edSquareD5, uMinorB5)), X.*(edSquareD5, uMinorD5))
					  val dD6 = X.-(dD4, dD5)
					  val dD7 = X.+(dD3, dD6)
					      
				      in
					  X.sign(X.+(dB7, dD7))
				      end
				  
			 end
		     
	    end
	
    end


    val smlStagedInSphere3 =
let val E = R.fromManExp{man=1.0, exp= ~53}
    val rm = IEEEReal.getRoundingMode()
    val _ = IEEEReal.setRoundingMode IEEEReal.TO_POSINF
val errA = (15.0 + 151.0*E) * E 
val errB = (5.0 + 63.0*E) * E 
val errC = (76.0 + 1269.0*E) * E * E 
val errCX = (2.0 + 7.0*E) * E 
    val _ = IEEEReal.setRoundingMode rm
in
fn [ ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz ] =>
let val adx = R.-(ax, dx)
val bdx = R.-(bx, dx)
val cdx = R.-(cx, dx)
val ady = R.-(ay, dy)
val bdy = R.-(by, dy)
val cdy = R.-(cy, dy)
val adz = R.-(az, dz)
val bdz = R.-(bz, dz)
val cdz = R.-(cz, dz)
val adSquare = R.*(adx, adx)
val adSquare2 = R.*(ady, ady)
val adSquare3 = R.*(adz, adz)
val adSquare4 = R.+(adSquare2, adSquare3)
val adSquare5 = R.+(adSquare, adSquare4)
val bdSquare = R.*(bdx, bdx)
val bdSquare2 = R.*(bdy, bdy)
val bdSquare3 = R.*(bdz, bdz)
val bdSquare4 = R.+(bdSquare2, bdSquare3)
val bdSquare5 = R.+(bdSquare, bdSquare4)
val cdSquare = R.*(cdx, cdx)
val cdSquare2 = R.*(cdy, cdy)
val cdSquare3 = R.*(cdz, cdz)
val cdSquare4 = R.+(cdSquare2, cdSquare3)
val cdSquare5 = R.+(cdSquare, cdSquare4)
val abxy = R.*(adx, bdy)
val abxy2 = R.*(bdx, ady)
val abxy3 = R.-(abxy, abxy2)
val abxyP3 = R.+(R.abs(abxy), R.abs(abxy2))
val abyz = R.*(ady, bdz)
val abyz2 = R.*(bdy, adz)
val abyz3 = R.-(abyz, abyz2)
val abyzP3 = R.+(R.abs(abyz), R.abs(abyz2))
val abxz = R.*(adx, bdz)
val abxz2 = R.*(bdx, adz)
val abxz3 = R.-(abxz, abxz2)
val abxzP3 = R.+(R.abs(abxz), R.abs(abxz2))
val bcxy = R.*(bdx, cdy)
val bcxy2 = R.*(cdx, bdy)
val bcxy3 = R.-(bcxy, bcxy2)
val bcxyP3 = R.+(R.abs(bcxy), R.abs(bcxy2))
val bcyz = R.*(bdy, cdz)
val bcyz2 = R.*(cdy, bdz)
val bcyz3 = R.-(bcyz, bcyz2)
val bcyzP3 = R.+(R.abs(bcyz), R.abs(bcyz2))
val bcxz = R.*(bdx, cdz)
val bcxz2 = R.*(cdx, bdz)
val bcxz3 = R.-(bcxz, bcxz2)
val bcxzP3 = R.+(R.abs(bcxz), R.abs(bcxz2))
val caxy = R.*(cdx, ady)
val caxy2 = R.*(adx, cdy)
val caxy3 = R.-(caxy, caxy2)
val caxyP3 = R.+(R.abs(caxy), R.abs(caxy2))
val cayz = R.*(cdy, adz)
val cayz2 = R.*(ady, cdz)
val cayz3 = R.-(cayz, cayz2)
val cayzP3 = R.+(R.abs(cayz), R.abs(cayz2))
val caxz = R.*(cdx, adz)
val caxz2 = R.*(adx, cdz)
val caxz3 = R.-(caxz, caxz2)
val caxzP3 = R.+(R.abs(caxz), R.abs(caxz2))
val xMinor = R.*(bcyz3, adSquare5)
val xMinorP = R.*(bcyzP3, adSquare5)
val xMinor2 = R.*(cayz3, bdSquare5)
val xMinorP2 = R.*(cayzP3, bdSquare5)
val xMinor3 = R.*(abyz3, cdSquare5)
val xMinorP3 = R.*(abyzP3, cdSquare5)
val xMinor4 = R.+(xMinor2, xMinor3)
val xMinorP4 = R.+(xMinorP2, xMinorP3)
val xMinor5 = R.+(xMinor, xMinor4)
val xMinorP5 = R.+(xMinorP, xMinorP4)
val yMinor = R.*(bcxz3, adSquare5)
val yMinorP = R.*(bcxzP3, adSquare5)
val yMinor2 = R.*(caxz3, bdSquare5)
val yMinorP2 = R.*(caxzP3, bdSquare5)
val yMinor3 = R.*(abxz3, cdSquare5)
val yMinorP3 = R.*(abxzP3, cdSquare5)
val yMinor4 = R.+(yMinor2, yMinor3)
val yMinorP4 = R.+(yMinorP2, yMinorP3)
val yMinor5 = R.+(yMinor, yMinor4)
val yMinorP5 = R.+(yMinorP, yMinorP4)
val zMinor = R.*(bcxy3, adSquare5)
val zMinorP = R.*(bcxyP3, adSquare5)
val zMinor2 = R.*(caxy3, bdSquare5)
val zMinorP2 = R.*(caxyP3, bdSquare5)
val zMinor3 = R.*(abxy3, cdSquare5)
val zMinorP3 = R.*(abxyP3, cdSquare5)
val zMinor4 = R.+(zMinor2, zMinor3)
val zMinorP4 = R.+(zMinorP2, zMinorP3)
val zMinor5 = R.+(zMinor, zMinor4)
val zMinorP5 = R.+(zMinorP, zMinorP4)
val uMinor = R.*(adx, bcyz3)
val uMinorP = R.*(R.abs(adx), bcyzP3)
val uMinor2 = R.*(bdx, cayz3)
val uMinorP2 = R.*(R.abs(bdx), cayzP3)
val uMinor3 = R.*(cdx, abyz3)
val uMinorP3 = R.*(R.abs(cdx), abyzP3)
val uMinor4 = R.+(uMinor2, uMinor3)
val uMinorP4 = R.+(uMinorP2, uMinorP3)
val uMinor5 = R.+(uMinor, uMinor4)
val uMinorP5 = R.+(uMinorP, uMinorP4)
val suspB2 = susp (fn () => 
let 
val adSquareB = Q.sq(adx)
val adSquareB2 = Q.sq(ady)
val adSquareB3 = Q.sq(adz)
val adSquareB4 = Q.+(adSquareB2, adSquareB3)
val adSquareB5 = Q.add2(adSquareB4, adSquareB)
val bdSquareB = Q.sq(bdx)
val bdSquareB2 = Q.sq(bdy)
val bdSquareB3 = Q.sq(bdz)
val bdSquareB4 = Q.+(bdSquareB2, bdSquareB3)
val bdSquareB5 = Q.add2(bdSquareB4, bdSquareB)
val cdSquareB = Q.sq(cdx)
val cdSquareB2 = Q.sq(cdy)
val cdSquareB3 = Q.sq(cdz)
val cdSquareB4 = Q.+(cdSquareB2, cdSquareB3)
val cdSquareB5 = Q.add2(cdSquareB4, cdSquareB)
val abxyB = Q.prod(adx, bdy)
val abxyB2 = Q.prod(bdx, ady)
val abxyB3 = Q.-(abxyB, abxyB2)
val abyzB = Q.prod(ady, bdz)
val abyzB2 = Q.prod(bdy, adz)
val abyzB3 = Q.-(abyzB, abyzB2)
val abxzB = Q.prod(adx, bdz)
val abxzB2 = Q.prod(bdx, adz)
val abxzB3 = Q.-(abxzB, abxzB2)
val bcxyB = Q.prod(bdx, cdy)
val bcxyB2 = Q.prod(cdx, bdy)
val bcxyB3 = Q.-(bcxyB, bcxyB2)
val bcyzB = Q.prod(bdy, cdz)
val bcyzB2 = Q.prod(cdy, bdz)
val bcyzB3 = Q.-(bcyzB, bcyzB2)
val bcxzB = Q.prod(bdx, cdz)
val bcxzB2 = Q.prod(cdx, bdz)
val bcxzB3 = Q.-(bcxzB, bcxzB2)
val caxyB = Q.prod(cdx, ady)
val caxyB2 = Q.prod(adx, cdy)
val caxyB3 = Q.-(caxyB, caxyB2)
val cayzB = Q.prod(cdy, adz)
val cayzB2 = Q.prod(ady, cdz)
val cayzB3 = Q.-(cayzB, cayzB2)
val caxzB = Q.prod(cdx, adz)
val caxzB2 = Q.prod(adx, cdz)
val caxzB3 = Q.-(caxzB, caxzB2)
val xMinorB = X.*(bcyzB3, adSquareB5)
val xMinorB2 = X.*(cayzB3, bdSquareB5)
val xMinorB3 = X.*(abyzB3, cdSquareB5)
val xMinorB4 = X.+(xMinorB2, xMinorB3)
val xMinorB5 = X.+(xMinorB, xMinorB4)
val yMinorB = X.*(bcxzB3, adSquareB5)
val yMinorB2 = X.*(caxzB3, bdSquareB5)
val yMinorB3 = X.*(abxzB3, cdSquareB5)
val yMinorB4 = X.+(yMinorB2, yMinorB3)
val yMinorB5 = X.+(yMinorB, yMinorB4)
val zMinorB = X.*(bcxyB3, adSquareB5)
val zMinorB2 = X.*(caxyB3, bdSquareB5)
val zMinorB3 = X.*(abxyB3, cdSquareB5)
val zMinorB4 = X.+(zMinorB2, zMinorB3)
val zMinorB5 = X.+(zMinorB, zMinorB4)
val uMinorB = X.scale(adx, bcyzB3)
val uMinorB2 = X.scale(bdx, cayzB3)
val uMinorB3 = X.scale(cdx, abyzB3)
val uMinorB4 = X.+(uMinorB2, uMinorB3)
val uMinorB5 = X.+(uMinorB, uMinorB4)

in
((abxyB3, abxzB3, abyzB3, adSquareB5, bcxyB3, bcxzB3, bcyzB3, bdSquareB5, caxyB3, caxzB3, cayzB3, cdSquareB5), (uMinorB5, xMinorB5, yMinorB5, zMinorB5))
end)

val suspC2 = susp (fn () => 
let 
val adxC = #err (Q.toResErr(Q.diff(ax, dx)))
val bdxC = #err (Q.toResErr(Q.diff(bx, dx)))
val cdxC = #err (Q.toResErr(Q.diff(cx, dx)))
val adyC = #err (Q.toResErr(Q.diff(ay, dy)))
val bdyC = #err (Q.toResErr(Q.diff(by, dy)))
val cdyC = #err (Q.toResErr(Q.diff(cy, dy)))
val adzC = #err (Q.toResErr(Q.diff(az, dz)))
val bdzC = #err (Q.toResErr(Q.diff(bz, dz)))
val cdzC = #err (Q.toResErr(Q.diff(cz, dz)))
val adSquareC = R.*(2.0, R.*(adx, adxC))
val adSquareC2 = R.*(2.0, R.*(ady, adyC))
val adSquareC3 = R.*(2.0, R.*(adz, adzC))
val adSquareC4 = R.+(adSquareC2, adSquareC3)
val adSquareC5 = R.+(adSquareC, adSquareC4)
val bdSquareC = R.*(2.0, R.*(bdx, bdxC))
val bdSquareC2 = R.*(2.0, R.*(bdy, bdyC))
val bdSquareC3 = R.*(2.0, R.*(bdz, bdzC))
val bdSquareC4 = R.+(bdSquareC2, bdSquareC3)
val bdSquareC5 = R.+(bdSquareC, bdSquareC4)
val cdSquareC = R.*(2.0, R.*(cdx, cdxC))
val cdSquareC2 = R.*(2.0, R.*(cdy, cdyC))
val cdSquareC3 = R.*(2.0, R.*(cdz, cdzC))
val cdSquareC4 = R.+(cdSquareC2, cdSquareC3)
val cdSquareC5 = R.+(cdSquareC, cdSquareC4)
val abxyC = R.+(R.*(adx, bdyC), R.*(adxC, bdy))
val abxyC2 = R.+(R.*(bdx, adyC), R.*(bdxC, ady))
val abxyC3 = R.-(abxyC, abxyC2)
val abyzC = R.+(R.*(ady, bdzC), R.*(adyC, bdz))
val abyzC2 = R.+(R.*(bdy, adzC), R.*(bdyC, adz))
val abyzC3 = R.-(abyzC, abyzC2)
val abxzC = R.+(R.*(adx, bdzC), R.*(adxC, bdz))
val abxzC2 = R.+(R.*(bdx, adzC), R.*(bdxC, adz))
val abxzC3 = R.-(abxzC, abxzC2)
val bcxyC = R.+(R.*(bdx, cdyC), R.*(bdxC, cdy))
val bcxyC2 = R.+(R.*(cdx, bdyC), R.*(cdxC, bdy))
val bcxyC3 = R.-(bcxyC, bcxyC2)
val bcyzC = R.+(R.*(bdy, cdzC), R.*(bdyC, cdz))
val bcyzC2 = R.+(R.*(cdy, bdzC), R.*(cdyC, bdz))
val bcyzC3 = R.-(bcyzC, bcyzC2)
val bcxzC = R.+(R.*(bdx, cdzC), R.*(bdxC, cdz))
val bcxzC2 = R.+(R.*(cdx, bdzC), R.*(cdxC, bdz))
val bcxzC3 = R.-(bcxzC, bcxzC2)
val caxyC = R.+(R.*(cdx, adyC), R.*(cdxC, ady))
val caxyC2 = R.+(R.*(adx, cdyC), R.*(adxC, cdy))
val caxyC3 = R.-(caxyC, caxyC2)
val cayzC = R.+(R.*(cdy, adzC), R.*(cdyC, adz))
val cayzC2 = R.+(R.*(ady, cdzC), R.*(adyC, cdz))
val cayzC3 = R.-(cayzC, cayzC2)
val caxzC = R.+(R.*(cdx, adzC), R.*(cdxC, adz))
val caxzC2 = R.+(R.*(adx, cdzC), R.*(adxC, cdz))
val caxzC3 = R.-(caxzC, caxzC2)
val xMinorC = R.+(R.*(bcyz3, adSquareC5), R.*(bcyzC3, adSquare5))
val xMinorC2 = R.+(R.*(cayz3, bdSquareC5), R.*(cayzC3, bdSquare5))
val xMinorC3 = R.+(R.*(abyz3, cdSquareC5), R.*(abyzC3, cdSquare5))
val xMinorC4 = R.+(xMinorC2, xMinorC3)
val xMinorC5 = R.+(xMinorC, xMinorC4)
val yMinorC = R.+(R.*(bcxz3, adSquareC5), R.*(bcxzC3, adSquare5))
val yMinorC2 = R.+(R.*(caxz3, bdSquareC5), R.*(caxzC3, bdSquare5))
val yMinorC3 = R.+(R.*(abxz3, cdSquareC5), R.*(abxzC3, cdSquare5))
val yMinorC4 = R.+(yMinorC2, yMinorC3)
val yMinorC5 = R.+(yMinorC, yMinorC4)
val zMinorC = R.+(R.*(bcxy3, adSquareC5), R.*(bcxyC3, adSquare5))
val zMinorC2 = R.+(R.*(caxy3, bdSquareC5), R.*(caxyC3, bdSquare5))
val zMinorC3 = R.+(R.*(abxy3, cdSquareC5), R.*(abxyC3, cdSquare5))
val zMinorC4 = R.+(zMinorC2, zMinorC3)
val zMinorC5 = R.+(zMinorC, zMinorC4)
val uMinorC = R.+(R.*(adx, bcyzC3), R.*(adxC, bcyz3))
val uMinorC2 = R.+(R.*(bdx, cayzC3), R.*(bdxC, cayz3))
val uMinorC3 = R.+(R.*(cdx, abyzC3), R.*(cdxC, abyz3))
val uMinorC4 = R.+(uMinorC2, uMinorC3)
val uMinorC5 = R.+(uMinorC, uMinorC4)

in
((adxC, adyC, adzC, bdxC, bdyC, bdzC, cdxC, cdyC, cdzC), (uMinorC5, xMinorC5, yMinorC5, zMinorC5))
end)

val suspD2 = susp (fn () => 
let 
val ((abxyB3, abxzB3, abyzB3, adSquareB5, bcxyB3, bcxzB3, bcyzB3, bdSquareB5, caxyB3, caxzB3, cayzB3, cdSquareB5), _) = suspB2()

val ((adxC, adyC, adzC, bdxC, bdyC, bdzC, cdxC, cdyC, cdzC), _) = suspC2()

val adSquareD = Q.+(Q.double(Q.prod(adx, adxC)), Q.sq(adxC))
val adSquareD2 = Q.+(Q.double(Q.prod(ady, adyC)), Q.sq(adyC))
val adSquareD3 = Q.+(Q.double(Q.prod(adz, adzC)), Q.sq(adzC))
val adSquareD4 = X.+(adSquareD2, adSquareD3)
val adSquareD5 = X.+(adSquareD, adSquareD4)
val bdSquareD = Q.+(Q.double(Q.prod(bdx, bdxC)), Q.sq(bdxC))
val bdSquareD2 = Q.+(Q.double(Q.prod(bdy, bdyC)), Q.sq(bdyC))
val bdSquareD3 = Q.+(Q.double(Q.prod(bdz, bdzC)), Q.sq(bdzC))
val bdSquareD4 = X.+(bdSquareD2, bdSquareD3)
val bdSquareD5 = X.+(bdSquareD, bdSquareD4)
val cdSquareD = Q.+(Q.double(Q.prod(cdx, cdxC)), Q.sq(cdxC))
val cdSquareD2 = Q.+(Q.double(Q.prod(cdy, cdyC)), Q.sq(cdyC))
val cdSquareD3 = Q.+(Q.double(Q.prod(cdz, cdzC)), Q.sq(cdzC))
val cdSquareD4 = X.+(cdSquareD2, cdSquareD3)
val cdSquareD5 = X.+(cdSquareD, cdSquareD4)
val abxyD = Q.add2(Q.+(Q.prod(adx, bdyC), Q.prod(adxC, bdy)), Q.prod(adxC, bdyC))
val abxyD2 = Q.add2(Q.+(Q.prod(bdx, adyC), Q.prod(bdxC, ady)), Q.prod(bdxC, adyC))
val abxyD3 = X.-(abxyD, abxyD2)
val abyzD = Q.add2(Q.+(Q.prod(ady, bdzC), Q.prod(adyC, bdz)), Q.prod(adyC, bdzC))
val abyzD2 = Q.add2(Q.+(Q.prod(bdy, adzC), Q.prod(bdyC, adz)), Q.prod(bdyC, adzC))
val abyzD3 = X.-(abyzD, abyzD2)
val abxzD = Q.add2(Q.+(Q.prod(adx, bdzC), Q.prod(adxC, bdz)), Q.prod(adxC, bdzC))
val abxzD2 = Q.add2(Q.+(Q.prod(bdx, adzC), Q.prod(bdxC, adz)), Q.prod(bdxC, adzC))
val abxzD3 = X.-(abxzD, abxzD2)
val bcxyD = Q.add2(Q.+(Q.prod(bdx, cdyC), Q.prod(bdxC, cdy)), Q.prod(bdxC, cdyC))
val bcxyD2 = Q.add2(Q.+(Q.prod(cdx, bdyC), Q.prod(cdxC, bdy)), Q.prod(cdxC, bdyC))
val bcxyD3 = X.-(bcxyD, bcxyD2)
val bcyzD = Q.add2(Q.+(Q.prod(bdy, cdzC), Q.prod(bdyC, cdz)), Q.prod(bdyC, cdzC))
val bcyzD2 = Q.add2(Q.+(Q.prod(cdy, bdzC), Q.prod(cdyC, bdz)), Q.prod(cdyC, bdzC))
val bcyzD3 = X.-(bcyzD, bcyzD2)
val bcxzD = Q.add2(Q.+(Q.prod(bdx, cdzC), Q.prod(bdxC, cdz)), Q.prod(bdxC, cdzC))
val bcxzD2 = Q.add2(Q.+(Q.prod(cdx, bdzC), Q.prod(cdxC, bdz)), Q.prod(cdxC, bdzC))
val bcxzD3 = X.-(bcxzD, bcxzD2)
val caxyD = Q.add2(Q.+(Q.prod(cdx, adyC), Q.prod(cdxC, ady)), Q.prod(cdxC, adyC))
val caxyD2 = Q.add2(Q.+(Q.prod(adx, cdyC), Q.prod(adxC, cdy)), Q.prod(adxC, cdyC))
val caxyD3 = X.-(caxyD, caxyD2)
val cayzD = Q.add2(Q.+(Q.prod(cdy, adzC), Q.prod(cdyC, adz)), Q.prod(cdyC, adzC))
val cayzD2 = Q.add2(Q.+(Q.prod(ady, cdzC), Q.prod(adyC, cdz)), Q.prod(adyC, cdzC))
val cayzD3 = X.-(cayzD, cayzD2)
val caxzD = Q.add2(Q.+(Q.prod(cdx, adzC), Q.prod(cdxC, adz)), Q.prod(cdxC, adzC))
val caxzD2 = Q.add2(Q.+(Q.prod(adx, cdzC), Q.prod(adxC, cdz)), Q.prod(adxC, cdzC))
val caxzD3 = X.-(caxzD, caxzD2)
val xMinorD = X.+(X.+(X.*(bcyzB3, adSquareD5), X.*(bcyzD3, adSquareB5)), X.*(bcyzD3, adSquareD5))
val xMinorD2 = X.+(X.+(X.*(cayzB3, bdSquareD5), X.*(cayzD3, bdSquareB5)), X.*(cayzD3, bdSquareD5))
val xMinorD3 = X.+(X.+(X.*(abyzB3, cdSquareD5), X.*(abyzD3, cdSquareB5)), X.*(abyzD3, cdSquareD5))
val xMinorD4 = X.+(xMinorD2, xMinorD3)
val xMinorD5 = X.+(xMinorD, xMinorD4)
val yMinorD = X.+(X.+(X.*(bcxzB3, adSquareD5), X.*(bcxzD3, adSquareB5)), X.*(bcxzD3, adSquareD5))
val yMinorD2 = X.+(X.+(X.*(caxzB3, bdSquareD5), X.*(caxzD3, bdSquareB5)), X.*(caxzD3, bdSquareD5))
val yMinorD3 = X.+(X.+(X.*(abxzB3, cdSquareD5), X.*(abxzD3, cdSquareB5)), X.*(abxzD3, cdSquareD5))
val yMinorD4 = X.+(yMinorD2, yMinorD3)
val yMinorD5 = X.+(yMinorD, yMinorD4)
val zMinorD = X.+(X.+(X.*(bcxyB3, adSquareD5), X.*(bcxyD3, adSquareB5)), X.*(bcxyD3, adSquareD5))
val zMinorD2 = X.+(X.+(X.*(caxyB3, bdSquareD5), X.*(caxyD3, bdSquareB5)), X.*(caxyD3, bdSquareD5))
val zMinorD3 = X.+(X.+(X.*(abxyB3, cdSquareD5), X.*(abxyD3, cdSquareB5)), X.*(abxyD3, cdSquareD5))
val zMinorD4 = X.+(zMinorD2, zMinorD3)
val zMinorD5 = X.+(zMinorD, zMinorD4)
val uMinorD = X.+(X.+(X.scale(adx, bcyzD3), X.scale(adxC, bcyzB3)), X.scale(adxC, bcyzD3))
val uMinorD2 = X.+(X.+(X.scale(bdx, cayzD3), X.scale(bdxC, cayzB3)), X.scale(bdxC, cayzD3))
val uMinorD3 = X.+(X.+(X.scale(cdx, abyzD3), X.scale(cdxC, abyzB3)), X.scale(cdxC, abyzD3))
val uMinorD4 = X.+(uMinorD2, uMinorD3)
val uMinorD5 = X.+(uMinorD, uMinorD4)

in
(uMinorD5, xMinorD5, yMinorD5, zMinorD5)
end)


in
fn [ ex, ey, ez ] =>
let val edx = R.-(ex, dx)
val edy = R.-(ey, dy)
val edz = R.-(ez, dz)
val edSquare = R.*(edx, edx)
val edSquare2 = R.*(edy, edy)
val edSquare3 = R.*(edz, edz)
val edSquare4 = R.+(edSquare2, edSquare3)
val edSquare5 = R.+(edSquare, edSquare4)
val d = R.*(edx, xMinor5)
val dP = R.*(R.abs(edx), xMinorP5)
val d2 = R.*(edy, yMinor5)
val dP2 = R.*(R.abs(edy), yMinorP5)
val d3 = R.-(d, d2)
val dP3 = R.+(dP, dP2)
val d4 = R.*(edz, zMinor5)
val dP4 = R.*(R.abs(edz), zMinorP5)
val d5 = R.*(edSquare5, uMinor5)
val dP5 = R.*(edSquare5, uMinorP5)
val d6 = R.-(d4, d5)
val dP6 = R.+(dP4, dP5)
val d7 = R.+(d3, d6)
val dP7 = R.+(dP3, dP6)
val yAE = R.*(errA, dP7)

in

if d7 > yAE then 1
else if R.~(d7) > yAE then ~1
     else 
let val (_, (uMinorB5, xMinorB5, yMinorB5, zMinorB5)) = suspB2()

val edSquareB = Q.sq(edx)
val edSquareB2 = Q.sq(edy)
val edSquareB3 = Q.sq(edz)
val edSquareB4 = Q.+(edSquareB2, edSquareB3)
val edSquareB5 = Q.add2(edSquareB4, edSquareB)
val dB = X.scale(edx, xMinorB5)
val dB2 = X.scale(edy, yMinorB5)
val dB3 = X.-(dB, dB2)
val dB4 = X.scale(edz, zMinorB5)
val dB5 = X.*(edSquareB5, uMinorB5)
val dB6 = X.-(dB4, dB5)
val dB7 = X.+(dB3, dB6)
val yBX = X.approx(dB7)
val yBE = R.*(errB, dP7)

in
if yBX > yBE then 1
else if R.~(yBX) > yBE then ~1
     else 
let val (_, (uMinorC5, xMinorC5, yMinorC5, zMinorC5)) = suspC2()

val edxC = #err (Q.toResErr(Q.diff(ex, dx)))
val edyC = #err (Q.toResErr(Q.diff(ey, dy)))
val edzC = #err (Q.toResErr(Q.diff(ez, dz)))
val edSquareC = R.*(2.0, R.*(edx, edxC))
val edSquareC2 = R.*(2.0, R.*(edy, edyC))
val edSquareC3 = R.*(2.0, R.*(edz, edzC))
val edSquareC4 = R.+(edSquareC2, edSquareC3)
val edSquareC5 = R.+(edSquareC, edSquareC4)
val dC = R.+(R.*(edx, xMinorC5), R.*(edxC, xMinor5))
val dC2 = R.+(R.*(edy, yMinorC5), R.*(edyC, yMinor5))
val dC3 = R.-(dC, dC2)
val dC4 = R.+(R.*(edz, zMinorC5), R.*(edzC, zMinor5))
val dC5 = R.+(R.*(edSquare5, uMinorC5), R.*(edSquareC5, uMinor5))
val dC6 = R.-(dC4, dC5)
val dC7 = R.+(dC3, dC6)
val yCX = R.+(yBX, dC7)
val yCE = R.+(R.*(errCX, yBX), R.*(errC, dP7))

in
if yCX > yCE then 1
else if R.~(yCX) > yCE then ~1
     else 
let val (uMinorD5, xMinorD5, yMinorD5, zMinorD5) = suspD2()

val edSquareD = Q.+(Q.double(Q.prod(edx, edxC)), Q.sq(edxC))
val edSquareD2 = Q.+(Q.double(Q.prod(edy, edyC)), Q.sq(edyC))
val edSquareD3 = Q.+(Q.double(Q.prod(edz, edzC)), Q.sq(edzC))
val edSquareD4 = X.+(edSquareD2, edSquareD3)
val edSquareD5 = X.+(edSquareD, edSquareD4)
val dD = X.+(X.+(X.scale(edx, xMinorD5), X.scale(edxC, xMinorB5)), X.scale(edxC, xMinorD5))
val dD2 = X.+(X.+(X.scale(edy, yMinorD5), X.scale(edyC, yMinorB5)), X.scale(edyC, yMinorD5))
val dD3 = X.-(dD, dD2)
val dD4 = X.+(X.+(X.scale(edz, zMinorD5), X.scale(edzC, zMinorB5)), X.scale(edzC, zMinorD5))
val dD5 = X.+(X.+(X.*(edSquareB5, uMinorD5), X.*(edSquareD5, uMinorB5)), X.*(edSquareD5, uMinorD5))
val dD6 = X.-(dD4, dD5)
val dD7 = X.+(dD3, dD6)

in
X.sign(X.+(dB7, dD7))
end

end

end

end
end
end (* main let *)




    val smlInSphere3'' = 
let val E = R.fromManExp{man=1.0, exp= ~53}
    val rm = IEEEReal.getRoundingMode()
    val _ = IEEEReal.setRoundingMode IEEEReal.TO_POSINF
val errA = (15.0 + 151.0*E) * E 
val errB = (5.0 + 60.0*E) * E 
val errC = (75.0 + 1296.0*E) * E * E 
val errCX = (2.0 + 7.0*E) * E 
    val _ = IEEEReal.setRoundingMode rm
in
fn [ ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz, ex, ey, ez ] =>
let val aex = R.-(ax, ex)
val bex = R.-(bx, ex)
val cex = R.-(cx, ex)
val dex = R.-(dx, ex)
val aey = R.-(ay, ey)
val bey = R.-(by, ey)
val cey = R.-(cy, ey)
val dey = R.-(dy, ey)
val aez = R.-(az, ez)
val bez = R.-(bz, ez)
val cez = R.-(cz, ez)
val dez = R.-(dz, ez)
val aeSquare = R.*(aex, aex)
val aeSquare2 = R.*(aey, aey)
val aeSquare3 = R.*(aez, aez)
val aeSquare4 = R.+(aeSquare2, aeSquare3)
val aeSquare5 = R.+(aeSquare, aeSquare4)
val beSquare = R.*(bex, bex)
val beSquare2 = R.*(bey, bey)
val beSquare3 = R.*(bez, bez)
val beSquare4 = R.+(beSquare2, beSquare3)
val beSquare5 = R.+(beSquare, beSquare4)
val ceSquare = R.*(cex, cex)
val ceSquare2 = R.*(cey, cey)
val ceSquare3 = R.*(cez, cez)
val ceSquare4 = R.+(ceSquare2, ceSquare3)
val ceSquare5 = R.+(ceSquare, ceSquare4)
val deSquare = R.*(dex, dex)
val deSquare2 = R.*(dey, dey)
val deSquare3 = R.*(dez, dez)
val deSquare4 = R.+(deSquare2, deSquare3)
val deSquare5 = R.+(deSquare, deSquare4)
val abxy = R.*(aex, bey)
val abxy2 = R.*(aey, bex)
val abxy3 = R.-(abxy, abxy2)
val abxyP3 = R.+(R.abs(abxy), R.abs(abxy2))
val bcxy = R.*(bex, cey)
val bcxy2 = R.*(bey, cex)
val bcxy3 = R.-(bcxy, bcxy2)
val bcxyP3 = R.+(R.abs(bcxy), R.abs(bcxy2))
val cdxy = R.*(cex, dey)
val cdxy2 = R.*(cey, dex)
val cdxy3 = R.-(cdxy, cdxy2)
val cdxyP3 = R.+(R.abs(cdxy), R.abs(cdxy2))
val daxy = R.*(dex, aey)
val daxy2 = R.*(dey, aex)
val daxy3 = R.-(daxy, daxy2)
val daxyP3 = R.+(R.abs(daxy), R.abs(daxy2))
val acxy = R.*(aex, cey)
val acxy2 = R.*(aey, cex)
val acxy3 = R.-(acxy, acxy2)
val acxyP3 = R.+(R.abs(acxy), R.abs(acxy2))
val bdxy = R.*(bex, dey)
val bdxy2 = R.*(bey, dex)
val bdxy3 = R.-(bdxy, bdxy2)
val bdxyP3 = R.+(R.abs(bdxy), R.abs(bdxy2))
val aMinor = R.*(bez, cdxy3)
val aMinorP = R.*(R.abs(bez), cdxyP3)
val aMinor2 = R.*(cez, bdxy3)
val aMinorP2 = R.*(R.abs(cez), bdxyP3)
val aMinor3 = R.-(aMinor, aMinor2)
val aMinorP3 = R.+(aMinorP, aMinorP2)
val aMinor4 = R.*(dez, bcxy3)
val aMinorP4 = R.*(R.abs(dez), bcxyP3)
val aMinor5 = R.+(aMinor3, aMinor4)
val aMinorP5 = R.+(aMinorP3, aMinorP4)
val bMinor = R.*(aez, cdxy3)
val bMinorP = R.*(R.abs(aez), cdxyP3)
val bMinor2 = R.*(cez, daxy3)
val bMinorP2 = R.*(R.abs(cez), daxyP3)
val bMinor3 = R.*(dez, acxy3)
val bMinorP3 = R.*(R.abs(dez), acxyP3)
val bMinor4 = R.+(bMinor2, bMinor3)
val bMinorP4 = R.+(bMinorP2, bMinorP3)
val bMinor5 = R.+(bMinor, bMinor4)
val bMinorP5 = R.+(bMinorP, bMinorP4)
val cMinor = R.*(aez, bdxy3)
val cMinorP = R.*(R.abs(aez), bdxyP3)
val cMinor2 = R.*(bez, daxy3)
val cMinorP2 = R.*(R.abs(bez), daxyP3)
val cMinor3 = R.*(dez, abxy3)
val cMinorP3 = R.*(R.abs(dez), abxyP3)
val cMinor4 = R.+(cMinor2, cMinor3)
val cMinorP4 = R.+(cMinorP2, cMinorP3)
val cMinor5 = R.+(cMinor, cMinor4)
val cMinorP5 = R.+(cMinorP, cMinorP4)
val dMinor = R.*(aez, bcxy3)
val dMinorP = R.*(R.abs(aez), bcxyP3)
val dMinor2 = R.*(bez, acxy3)
val dMinorP2 = R.*(R.abs(bez), acxyP3)
val dMinor3 = R.-(dMinor, dMinor2)
val dMinorP3 = R.+(dMinorP, dMinorP2)
val dMinor4 = R.*(cez, abxy3)
val dMinorP4 = R.*(R.abs(cez), abxyP3)
val dMinor5 = R.+(dMinor3, dMinor4)
val dMinorP5 = R.+(dMinorP3, dMinorP4)
val aeSq = R.*(aex, aex)
val aeSq2 = R.*(aey, aey)
val aeSq3 = R.*(aez, aez)
val aeSq4 = R.+(aeSq2, aeSq3)
val aeSq5 = R.+(aeSq, aeSq4)
val beSq = R.*(bex, bex)
val beSq2 = R.*(bey, bey)
val beSq3 = R.*(bez, bez)
val beSq4 = R.+(beSq2, beSq3)
val beSq5 = R.+(beSq, beSq4)
val ceSq = R.*(cex, cex)
val ceSq2 = R.*(cey, cey)
val ceSq3 = R.*(cez, cez)
val ceSq4 = R.+(ceSq2, ceSq3)
val ceSq5 = R.+(ceSq, ceSq4)
val deSq = R.*(dex, dex)
val deSq2 = R.*(dey, dey)
val deSq3 = R.*(dez, dez)
val deSq4 = R.+(deSq2, deSq3)
val deSq5 = R.+(deSq, deSq4)
val d = R.*(aeSq5, aMinor5)
val dP = R.*(aeSq5, aMinorP5)
val d2 = R.*(beSq5, bMinor5)
val dP2 = R.*(beSq5, bMinorP5)
val d3 = R.-(d, d2)
val dP3 = R.+(dP, dP2)
val d4 = R.*(ceSq5, cMinor5)
val dP4 = R.*(ceSq5, cMinorP5)
val d5 = R.*(deSq5, dMinor5)
val dP5 = R.*(deSq5, dMinorP5)
val d6 = R.-(d4, d5)
val dP6 = R.+(dP4, dP5)
val d7 = R.+(d3, d6)
val dP7 = R.+(dP3, dP6)
val yAE = R.*(errA, dP7)

in

if d7 > yAE then 1
else if R.~(d7) > yAE then ~1
     else 
let 
val aeSquareB = Q.sq(aex)
val aeSquareB2 = Q.sq(aey)
val aeSquareB3 = Q.sq(aez)
val aeSquareB4 = Q.+(aeSquareB2, aeSquareB3)
val aeSquareB5 = Q.add2(aeSquareB4, aeSquareB)
val beSquareB = Q.sq(bex)
val beSquareB2 = Q.sq(bey)
val beSquareB3 = Q.sq(bez)
val beSquareB4 = Q.+(beSquareB2, beSquareB3)
val beSquareB5 = Q.add2(beSquareB4, beSquareB)
val ceSquareB = Q.sq(cex)
val ceSquareB2 = Q.sq(cey)
val ceSquareB3 = Q.sq(cez)
val ceSquareB4 = Q.+(ceSquareB2, ceSquareB3)
val ceSquareB5 = Q.add2(ceSquareB4, ceSquareB)
val deSquareB = Q.sq(dex)
val deSquareB2 = Q.sq(dey)
val deSquareB3 = Q.sq(dez)
val deSquareB4 = Q.+(deSquareB2, deSquareB3)
val deSquareB5 = Q.add2(deSquareB4, deSquareB)
val abxyB = Q.prod(aex, bey)
val abxyB2 = Q.prod(aey, bex)
val abxyB3 = Q.-(abxyB, abxyB2)
val bcxyB = Q.prod(bex, cey)
val bcxyB2 = Q.prod(bey, cex)
val bcxyB3 = Q.-(bcxyB, bcxyB2)
val cdxyB = Q.prod(cex, dey)
val cdxyB2 = Q.prod(cey, dex)
val cdxyB3 = Q.-(cdxyB, cdxyB2)
val daxyB = Q.prod(dex, aey)
val daxyB2 = Q.prod(dey, aex)
val daxyB3 = Q.-(daxyB, daxyB2)
val acxyB = Q.prod(aex, cey)
val acxyB2 = Q.prod(aey, cex)
val acxyB3 = Q.-(acxyB, acxyB2)
val bdxyB = Q.prod(bex, dey)
val bdxyB2 = Q.prod(bey, dex)
val bdxyB3 = Q.-(bdxyB, bdxyB2)
val aMinorB = X.scale(bez, cdxyB3)
val aMinorB2 = X.scale(cez, bdxyB3)
val aMinorB3 = X.-(aMinorB, aMinorB2)
val aMinorB4 = X.scale(dez, bcxyB3)
val aMinorB5 = X.+(aMinorB3, aMinorB4)
val bMinorB = X.scale(aez, cdxyB3)
val bMinorB2 = X.scale(cez, daxyB3)
val bMinorB3 = X.scale(dez, acxyB3)
val bMinorB4 = X.+(bMinorB2, bMinorB3)
val bMinorB5 = X.+(bMinorB, bMinorB4)
val cMinorB = X.scale(aez, bdxyB3)
val cMinorB2 = X.scale(bez, daxyB3)
val cMinorB3 = X.scale(dez, abxyB3)
val cMinorB4 = X.+(cMinorB2, cMinorB3)
val cMinorB5 = X.+(cMinorB, cMinorB4)
val dMinorB = X.scale(aez, bcxyB3)
val dMinorB2 = X.scale(bez, acxyB3)
val dMinorB3 = X.-(dMinorB, dMinorB2)
val dMinorB4 = X.scale(cez, abxyB3)
val dMinorB5 = X.+(dMinorB3, dMinorB4)
val aeSqB = Q.sq(aex)
val aeSqB2 = Q.sq(aey)
val aeSqB3 = Q.sq(aez)
val aeSqB4 = Q.+(aeSqB2, aeSqB3)
val aeSqB5 = Q.add2(aeSqB4, aeSqB)
val beSqB = Q.sq(bex)
val beSqB2 = Q.sq(bey)
val beSqB3 = Q.sq(bez)
val beSqB4 = Q.+(beSqB2, beSqB3)
val beSqB5 = Q.add2(beSqB4, beSqB)
val ceSqB = Q.sq(cex)
val ceSqB2 = Q.sq(cey)
val ceSqB3 = Q.sq(cez)
val ceSqB4 = Q.+(ceSqB2, ceSqB3)
val ceSqB5 = Q.add2(ceSqB4, ceSqB)
val deSqB = Q.sq(dex)
val deSqB2 = Q.sq(dey)
val deSqB3 = Q.sq(dez)
val deSqB4 = Q.+(deSqB2, deSqB3)
val deSqB5 = Q.add2(deSqB4, deSqB)
val dB = X.*(aeSqB5, aMinorB5)
val dB2 = X.*(beSqB5, bMinorB5)
val dB3 = X.-(dB, dB2)
val dB4 = X.*(ceSqB5, cMinorB5)
val dB5 = X.*(deSqB5, dMinorB5)
val dB6 = X.-(dB4, dB5)
val dB7 = X.+(dB3, dB6)
val yBX = X.approx(dB7)
val yBE = R.*(errB, dP7)

in
if yBX > yBE then 1
else if R.~(yBX) > yBE then ~1
     else 
let 
val aexC = #err (Q.toResErr(Q.diff(ax, ex)))
val bexC = #err (Q.toResErr(Q.diff(bx, ex)))
val cexC = #err (Q.toResErr(Q.diff(cx, ex)))
val dexC = #err (Q.toResErr(Q.diff(dx, ex)))
val aeyC = #err (Q.toResErr(Q.diff(ay, ey)))
val beyC = #err (Q.toResErr(Q.diff(by, ey)))
val ceyC = #err (Q.toResErr(Q.diff(cy, ey)))
val deyC = #err (Q.toResErr(Q.diff(dy, ey)))
val aezC = #err (Q.toResErr(Q.diff(az, ez)))
val bezC = #err (Q.toResErr(Q.diff(bz, ez)))
val cezC = #err (Q.toResErr(Q.diff(cz, ez)))
val dezC = #err (Q.toResErr(Q.diff(dz, ez)))
val aeSquareC = R.*(2.0, R.*(aex, aexC))
val aeSquareC2 = R.*(2.0, R.*(aey, aeyC))
val aeSquareC3 = R.*(2.0, R.*(aez, aezC))
val aeSquareC4 = R.+(aeSquareC2, aeSquareC3)
val aeSquareC5 = R.+(aeSquareC, aeSquareC4)
val beSquareC = R.*(2.0, R.*(bex, bexC))
val beSquareC2 = R.*(2.0, R.*(bey, beyC))
val beSquareC3 = R.*(2.0, R.*(bez, bezC))
val beSquareC4 = R.+(beSquareC2, beSquareC3)
val beSquareC5 = R.+(beSquareC, beSquareC4)
val ceSquareC = R.*(2.0, R.*(cex, cexC))
val ceSquareC2 = R.*(2.0, R.*(cey, ceyC))
val ceSquareC3 = R.*(2.0, R.*(cez, cezC))
val ceSquareC4 = R.+(ceSquareC2, ceSquareC3)
val ceSquareC5 = R.+(ceSquareC, ceSquareC4)
val deSquareC = R.*(2.0, R.*(dex, dexC))
val deSquareC2 = R.*(2.0, R.*(dey, deyC))
val deSquareC3 = R.*(2.0, R.*(dez, dezC))
val deSquareC4 = R.+(deSquareC2, deSquareC3)
val deSquareC5 = R.+(deSquareC, deSquareC4)
val abxyC = R.+(R.*(aex, beyC), R.*(aexC, bey))
val abxyC2 = R.+(R.*(aey, bexC), R.*(aeyC, bex))
val abxyC3 = R.-(abxyC, abxyC2)
val bcxyC = R.+(R.*(bex, ceyC), R.*(bexC, cey))
val bcxyC2 = R.+(R.*(bey, cexC), R.*(beyC, cex))
val bcxyC3 = R.-(bcxyC, bcxyC2)
val cdxyC = R.+(R.*(cex, deyC), R.*(cexC, dey))
val cdxyC2 = R.+(R.*(cey, dexC), R.*(ceyC, dex))
val cdxyC3 = R.-(cdxyC, cdxyC2)
val daxyC = R.+(R.*(dex, aeyC), R.*(dexC, aey))
val daxyC2 = R.+(R.*(dey, aexC), R.*(deyC, aex))
val daxyC3 = R.-(daxyC, daxyC2)
val acxyC = R.+(R.*(aex, ceyC), R.*(aexC, cey))
val acxyC2 = R.+(R.*(aey, cexC), R.*(aeyC, cex))
val acxyC3 = R.-(acxyC, acxyC2)
val bdxyC = R.+(R.*(bex, deyC), R.*(bexC, dey))
val bdxyC2 = R.+(R.*(bey, dexC), R.*(beyC, dex))
val bdxyC3 = R.-(bdxyC, bdxyC2)
val aMinorC = R.+(R.*(bez, cdxyC3), R.*(bezC, cdxy3))
val aMinorC2 = R.+(R.*(cez, bdxyC3), R.*(cezC, bdxy3))
val aMinorC3 = R.-(aMinorC, aMinorC2)
val aMinorC4 = R.+(R.*(dez, bcxyC3), R.*(dezC, bcxy3))
val aMinorC5 = R.+(aMinorC3, aMinorC4)
val bMinorC = R.+(R.*(aez, cdxyC3), R.*(aezC, cdxy3))
val bMinorC2 = R.+(R.*(cez, daxyC3), R.*(cezC, daxy3))
val bMinorC3 = R.+(R.*(dez, acxyC3), R.*(dezC, acxy3))
val bMinorC4 = R.+(bMinorC2, bMinorC3)
val bMinorC5 = R.+(bMinorC, bMinorC4)
val cMinorC = R.+(R.*(aez, bdxyC3), R.*(aezC, bdxy3))
val cMinorC2 = R.+(R.*(bez, daxyC3), R.*(bezC, daxy3))
val cMinorC3 = R.+(R.*(dez, abxyC3), R.*(dezC, abxy3))
val cMinorC4 = R.+(cMinorC2, cMinorC3)
val cMinorC5 = R.+(cMinorC, cMinorC4)
val dMinorC = R.+(R.*(aez, bcxyC3), R.*(aezC, bcxy3))
val dMinorC2 = R.+(R.*(bez, acxyC3), R.*(bezC, acxy3))
val dMinorC3 = R.-(dMinorC, dMinorC2)
val dMinorC4 = R.+(R.*(cez, abxyC3), R.*(cezC, abxy3))
val dMinorC5 = R.+(dMinorC3, dMinorC4)
val aeSqC = R.*(2.0, R.*(aex, aexC))
val aeSqC2 = R.*(2.0, R.*(aey, aeyC))
val aeSqC3 = R.*(2.0, R.*(aez, aezC))
val aeSqC4 = R.+(aeSqC2, aeSqC3)
val aeSqC5 = R.+(aeSqC, aeSqC4)
val beSqC = R.*(2.0, R.*(bex, bexC))
val beSqC2 = R.*(2.0, R.*(bey, beyC))
val beSqC3 = R.*(2.0, R.*(bez, bezC))
val beSqC4 = R.+(beSqC2, beSqC3)
val beSqC5 = R.+(beSqC, beSqC4)
val ceSqC = R.*(2.0, R.*(cex, cexC))
val ceSqC2 = R.*(2.0, R.*(cey, ceyC))
val ceSqC3 = R.*(2.0, R.*(cez, cezC))
val ceSqC4 = R.+(ceSqC2, ceSqC3)
val ceSqC5 = R.+(ceSqC, ceSqC4)
val deSqC = R.*(2.0, R.*(dex, dexC))
val deSqC2 = R.*(2.0, R.*(dey, deyC))
val deSqC3 = R.*(2.0, R.*(dez, dezC))
val deSqC4 = R.+(deSqC2, deSqC3)
val deSqC5 = R.+(deSqC, deSqC4)
val dC = R.+(R.*(aeSq5, aMinorC5), R.*(aeSqC5, aMinor5))
val dC2 = R.+(R.*(beSq5, bMinorC5), R.*(beSqC5, bMinor5))
val dC3 = R.-(dC, dC2)
val dC4 = R.+(R.*(ceSq5, cMinorC5), R.*(ceSqC5, cMinor5))
val dC5 = R.+(R.*(deSq5, dMinorC5), R.*(deSqC5, dMinor5))
val dC6 = R.-(dC4, dC5)
val dC7 = R.+(dC3, dC6)
val yCX = R.+(yBX, dC7)
val yCE = R.+(R.*(errCX, yBX), R.*(errC, dP7))

in
if yCX > yCE then 1
else if R.~(yCX) > yCE then ~1
     else 
let 
val aeSquareD = Q.+(Q.double(Q.prod(aex, aexC)), Q.sq(aexC))
val aeSquareD2 = Q.+(Q.double(Q.prod(aey, aeyC)), Q.sq(aeyC))
val aeSquareD3 = Q.+(Q.double(Q.prod(aez, aezC)), Q.sq(aezC))
val aeSquareD4 = X.+(aeSquareD2, aeSquareD3)
val aeSquareD5 = X.+(aeSquareD, aeSquareD4)
val beSquareD = Q.+(Q.double(Q.prod(bex, bexC)), Q.sq(bexC))
val beSquareD2 = Q.+(Q.double(Q.prod(bey, beyC)), Q.sq(beyC))
val beSquareD3 = Q.+(Q.double(Q.prod(bez, bezC)), Q.sq(bezC))
val beSquareD4 = X.+(beSquareD2, beSquareD3)
val beSquareD5 = X.+(beSquareD, beSquareD4)
val ceSquareD = Q.+(Q.double(Q.prod(cex, cexC)), Q.sq(cexC))
val ceSquareD2 = Q.+(Q.double(Q.prod(cey, ceyC)), Q.sq(ceyC))
val ceSquareD3 = Q.+(Q.double(Q.prod(cez, cezC)), Q.sq(cezC))
val ceSquareD4 = X.+(ceSquareD2, ceSquareD3)
val ceSquareD5 = X.+(ceSquareD, ceSquareD4)
val deSquareD = Q.+(Q.double(Q.prod(dex, dexC)), Q.sq(dexC))
val deSquareD2 = Q.+(Q.double(Q.prod(dey, deyC)), Q.sq(deyC))
val deSquareD3 = Q.+(Q.double(Q.prod(dez, dezC)), Q.sq(dezC))
val deSquareD4 = X.+(deSquareD2, deSquareD3)
val deSquareD5 = X.+(deSquareD, deSquareD4)
val abxyD = Q.add2(Q.+(Q.prod(aex, beyC), Q.prod(aexC, bey)), Q.prod(aexC, beyC))
val abxyD2 = Q.add2(Q.+(Q.prod(aey, bexC), Q.prod(aeyC, bex)), Q.prod(aeyC, bexC))
val abxyD3 = X.-(abxyD, abxyD2)
val bcxyD = Q.add2(Q.+(Q.prod(bex, ceyC), Q.prod(bexC, cey)), Q.prod(bexC, ceyC))
val bcxyD2 = Q.add2(Q.+(Q.prod(bey, cexC), Q.prod(beyC, cex)), Q.prod(beyC, cexC))
val bcxyD3 = X.-(bcxyD, bcxyD2)
val cdxyD = Q.add2(Q.+(Q.prod(cex, deyC), Q.prod(cexC, dey)), Q.prod(cexC, deyC))
val cdxyD2 = Q.add2(Q.+(Q.prod(cey, dexC), Q.prod(ceyC, dex)), Q.prod(ceyC, dexC))
val cdxyD3 = X.-(cdxyD, cdxyD2)
val daxyD = Q.add2(Q.+(Q.prod(dex, aeyC), Q.prod(dexC, aey)), Q.prod(dexC, aeyC))
val daxyD2 = Q.add2(Q.+(Q.prod(dey, aexC), Q.prod(deyC, aex)), Q.prod(deyC, aexC))
val daxyD3 = X.-(daxyD, daxyD2)
val acxyD = Q.add2(Q.+(Q.prod(aex, ceyC), Q.prod(aexC, cey)), Q.prod(aexC, ceyC))
val acxyD2 = Q.add2(Q.+(Q.prod(aey, cexC), Q.prod(aeyC, cex)), Q.prod(aeyC, cexC))
val acxyD3 = X.-(acxyD, acxyD2)
val bdxyD = Q.add2(Q.+(Q.prod(bex, deyC), Q.prod(bexC, dey)), Q.prod(bexC, deyC))
val bdxyD2 = Q.add2(Q.+(Q.prod(bey, dexC), Q.prod(beyC, dex)), Q.prod(beyC, dexC))
val bdxyD3 = X.-(bdxyD, bdxyD2)
val aMinorD = X.+(X.+(X.scale(bez, cdxyD3), X.scale(bezC, cdxyB3)), X.scale(bezC, cdxyD3))
val aMinorD2 = X.+(X.+(X.scale(cez, bdxyD3), X.scale(cezC, bdxyB3)), X.scale(cezC, bdxyD3))
val aMinorD3 = X.-(aMinorD, aMinorD2)
val aMinorD4 = X.+(X.+(X.scale(dez, bcxyD3), X.scale(dezC, bcxyB3)), X.scale(dezC, bcxyD3))
val aMinorD5 = X.+(aMinorD3, aMinorD4)
val bMinorD = X.+(X.+(X.scale(aez, cdxyD3), X.scale(aezC, cdxyB3)), X.scale(aezC, cdxyD3))
val bMinorD2 = X.+(X.+(X.scale(cez, daxyD3), X.scale(cezC, daxyB3)), X.scale(cezC, daxyD3))
val bMinorD3 = X.+(X.+(X.scale(dez, acxyD3), X.scale(dezC, acxyB3)), X.scale(dezC, acxyD3))
val bMinorD4 = X.+(bMinorD2, bMinorD3)
val bMinorD5 = X.+(bMinorD, bMinorD4)
val cMinorD = X.+(X.+(X.scale(aez, bdxyD3), X.scale(aezC, bdxyB3)), X.scale(aezC, bdxyD3))
val cMinorD2 = X.+(X.+(X.scale(bez, daxyD3), X.scale(bezC, daxyB3)), X.scale(bezC, daxyD3))
val cMinorD3 = X.+(X.+(X.scale(dez, abxyD3), X.scale(dezC, abxyB3)), X.scale(dezC, abxyD3))
val cMinorD4 = X.+(cMinorD2, cMinorD3)
val cMinorD5 = X.+(cMinorD, cMinorD4)
val dMinorD = X.+(X.+(X.scale(aez, bcxyD3), X.scale(aezC, bcxyB3)), X.scale(aezC, bcxyD3))
val dMinorD2 = X.+(X.+(X.scale(bez, acxyD3), X.scale(bezC, acxyB3)), X.scale(bezC, acxyD3))
val dMinorD3 = X.-(dMinorD, dMinorD2)
val dMinorD4 = X.+(X.+(X.scale(cez, abxyD3), X.scale(cezC, abxyB3)), X.scale(cezC, abxyD3))
val dMinorD5 = X.+(dMinorD3, dMinorD4)
val aeSqD = Q.+(Q.double(Q.prod(aex, aexC)), Q.sq(aexC))
val aeSqD2 = Q.+(Q.double(Q.prod(aey, aeyC)), Q.sq(aeyC))
val aeSqD3 = Q.+(Q.double(Q.prod(aez, aezC)), Q.sq(aezC))
val aeSqD4 = X.+(aeSqD2, aeSqD3)
val aeSqD5 = X.+(aeSqD, aeSqD4)
val beSqD = Q.+(Q.double(Q.prod(bex, bexC)), Q.sq(bexC))
val beSqD2 = Q.+(Q.double(Q.prod(bey, beyC)), Q.sq(beyC))
val beSqD3 = Q.+(Q.double(Q.prod(bez, bezC)), Q.sq(bezC))
val beSqD4 = X.+(beSqD2, beSqD3)
val beSqD5 = X.+(beSqD, beSqD4)
val ceSqD = Q.+(Q.double(Q.prod(cex, cexC)), Q.sq(cexC))
val ceSqD2 = Q.+(Q.double(Q.prod(cey, ceyC)), Q.sq(ceyC))
val ceSqD3 = Q.+(Q.double(Q.prod(cez, cezC)), Q.sq(cezC))
val ceSqD4 = X.+(ceSqD2, ceSqD3)
val ceSqD5 = X.+(ceSqD, ceSqD4)
val deSqD = Q.+(Q.double(Q.prod(dex, dexC)), Q.sq(dexC))
val deSqD2 = Q.+(Q.double(Q.prod(dey, deyC)), Q.sq(deyC))
val deSqD3 = Q.+(Q.double(Q.prod(dez, dezC)), Q.sq(dezC))
val deSqD4 = X.+(deSqD2, deSqD3)
val deSqD5 = X.+(deSqD, deSqD4)
val dD = X.+(X.+(X.*(aeSqB5, aMinorD5), X.*(aeSqD5, aMinorB5)), X.*(aeSqD5, aMinorD5))
val dD2 = X.+(X.+(X.*(beSqB5, bMinorD5), X.*(beSqD5, bMinorB5)), X.*(beSqD5, bMinorD5))
val dD3 = X.-(dD, dD2)
val dD4 = X.+(X.+(X.*(ceSqB5, cMinorD5), X.*(ceSqD5, cMinorB5)), X.*(ceSqD5, cMinorD5))
val dD5 = X.+(X.+(X.*(deSqB5, dMinorD5), X.*(deSqD5, dMinorB5)), X.*(deSqD5, dMinorD5))
val dD6 = X.-(dD4, dD5)
val dD7 = X.+(dD3, dD6)

in
X.sign(X.+(dB7, dD7))
end

end

end

end
end (* main let *)



    val smlStagedMinSphere3 = 
let val E = R.fromManExp{man=1.0, exp= ~53}
    val rm = IEEEReal.getRoundingMode()
    val _ = IEEEReal.setRoundingMode IEEEReal.TO_POSINF
val errA = (20.0 + 251.0*E) * E 
val errB = (6.0 + 92.0*E) * E 
val errC = (122.0 + 2559.0*E) * E * E 
val errCX = (2.0 + 7.0*E) * E 
    val _ = IEEEReal.setRoundingMode rm
in
fn [ ax, ay, az, bx, by, bz, cx, cy, cz ] =>
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
val ((abB5, acB5, bcB5, detBB2, detCB2), _) = suspB2()

val ((abxC, abyC, abzC, acxC, acyC, aczC), _) = suspC2()

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
fn [ px, py, pz ] =>
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
let val (_, (ccxB5, ccyB5, cczB5, detAB2)) = suspB2()

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
let val (_, (ccxC5, ccyC5, cczC5, detAC2)) = suspC2()

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
let val (ccxD5, ccyD5, cczD5, detAD2) = suspD2()

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
