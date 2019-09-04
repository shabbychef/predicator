(***************************************
 ** CompilerTest.sml
 ** sml
 ** 
 ** Aleksandar Nanevski
 **
 ** Test file for the compiler
 ***************************************)

structure Timing =
struct
    exception Nonexistent

    val lastTest = 16

    exception BadInput

    structure S = Source
    structure T = Target
    structure E = ExactPredicates

    structure Q = NormalizedRational
    structure I = RoundDownFloatInterval

    structure QX = ExtendedFloat
    structure X = XFloat

    local 
	fun tokenBreak #" " = true
	  | tokenBreak #"\t" = true
	  | tokenBreak _ = false
	    
	fun readReal s = 
	    case (Real.fromString s) of 
		SOME r => r
	      | NONE => raise BadInput
		    
	fun readInt s =
	    case (Int.fromString s) of
		SOME n => n
	      | NONE => raise BadInput
    in
	fun formatOrient2 line =
	    let val (i::ax::ay::bx::by::cx::cy::det::_) = 
		String.tokens tokenBreak line
	    in
		{num=readInt i,
		 a=(readReal ax, readReal ay),
		 b=(readReal bx, readReal by),
		 c=(readReal cx, readReal cy),
		 sign=readInt det}
	    end
	
	and formatOrient3 line =
	    let val (i::ax::ay::az::bx::by::bz::
		     cx::cy::cz::dx::dy::dz::det::_) = 
		String.tokens tokenBreak line
	    in
		{num=readInt i,
		 a=(readReal ax, readReal ay, readReal az),
		 b=(readReal bx, readReal by, readReal bz),
		 c=(readReal cx, readReal cy, readReal cz),
		 d=(readReal dx, readReal dy, readReal dz),
		 sign=readInt det}
	    end
	
	and formatInSphere2 line =
	    let val (i::ax::ay::bx::by::cx::cy::
		     dx::dy::det::_) = String.tokens tokenBreak line
	    in
		{num=readInt i,
		 a=(readReal ax, readReal ay),
		 b=(readReal bx, readReal by),
		 c=(readReal cx, readReal cy),
		 d=(readReal dx, readReal dy),
		 sign=readInt det}
	    end
	
	and formatInSphere3 line =
	    let val (i::ax::ay::az::bx::by::bz::
		     cx::cy::cz::dx::dy::dz::
		     ex::ey::ez::det::_) = String.tokens tokenBreak line
	    in
		{num=readInt i,
		 a=(readReal ax, readReal ay, readReal az),
		 b=(readReal bx, readReal by, readReal bz),
		 c=(readReal cx, readReal cy, readReal cz),
		 d=(readReal dx, readReal dy, readReal dz),
		 e=(readReal ex, readReal ey, readReal ez),
		 sign=readInt det}
	    end
    end


    fun go action infile =
	let fun loop "" = true
	      | loop line = 
	        if (action line) then loop (TextIO.inputLine infile)
		else false
	in
	    loop (TextIO.inputLine infile)
	end
    


    fun checkOrient2 line =
	let val {num=i, a=(ax, ay), b=(bx, by), 
		 c=(cx, cy), sign=sign} = formatOrient2 line
	    val r = ExactPredicates.smlOrient2 [ax, ay, bx, by, cx, cy]
	in
	    r = sign
	end

    
    fun checkInSphere2 line =
	let val {num=i,a=(ax, ay),b=(bx, by),
		 c=(cx, cy),d=(dx, dy),sign=sign} = formatInSphere2 line
	    val r = ExactPredicates.smlInSphere2 [ax, ay, bx, by, cx, cy, dx, dy]
	in
	    r = sign
	end


    fun checkOrient3 line =
	let val {num=i,a=(ax, ay, az),b=(bx, by, bz),
		 c=(cx, cy, cz),d=(dx, dy, dz),sign=sign} = formatOrient3 line
	    val r = ExactPredicates.smlOrient3 [ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz]
	in
	    r = sign
	end


    fun checkStagedOrient3 line =
	let val {num=i,a=(ax, ay, az),b=(bx, by, bz),
		 c=(cx, cy, cz),d=(dx, dy, dz),sign=sign} = formatOrient3 line
	    val r = ExactPredicates.smlStagedOrient3 [ax, ay, az, bx, by, bz, cx, cy, cz] [dx, dy, dz]
	in
	    r = sign
	end


    fun checkInSphere3 line =
	let val {num=i,a=(ax, ay, az),b=(bx, by, bz),
		 c=(cx, cy, cz),d=(dx, dy, dz),e=(ex, ey, ez),sign=sign} = formatInSphere3 line
	    val r = ExactPredicates.smlInSphere3 [ax, ay, az, bx, by, bz, cx, cy, cz, 
						  dx, dy, dz, ex, ey, ez]
	in
	    r = sign
	end


    fun checkInSphere3'' line =
	let val {num=i,a=(ax, ay, az),b=(bx, by, bz),
		 c=(cx, cy, cz),d=(dx, dy, dz),e=(ex, ey, ez),sign=sign} = formatInSphere3 line
	    val r = ExactPredicates.smlInSphere3'' [ax, ay, az, bx, by, bz, cx, cy, cz, 
						    dx, dy, dz, ex, ey, ez]
	in
	    r = r (* cause this is still incorrect predicate *)
	end


    fun checkStagedInSphere3 line =
	let val {num=i,a=(ax, ay, az),b=(bx, by, bz),
		 c=(cx, cy, cz),d=(dx, dy, dz),e=(ex, ey, ez),sign=sign} = formatInSphere3 line
	    val r = ExactPredicates.smlStagedInSphere3 [ax, ay, az, bx, by, bz, cx, cy, cz, 
							dx, dy, dz] [ex, ey, ez]
	    (* print for debugging purposes *)
	    (* val _ = print ((Int.toString i) ^ " " ^ 
			      (Int.toString sign) ^ " " ^ (Real.toString r) ^ "\n") *)
	in
	    r = sign
	end





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


    val eps2 = Real.fromManExp {man=1.0, exp= ~52}
    fun exfloatMinSphere3 ((ax', ay', az'), (bx', by', bz'), (cx', cy', cz')) =
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





    fun intervalMinSphere3 ((ax', ay', az'), (bx', by', bz'), (cx', cy', cz')) =
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
    

		

    fun checkRationalMinSphere3 line = 
	let val {num=i, a=(ax, ay, az), b=(bx, by, bz), c=(cx, cy, cz), 
		 d=(px, py, pz), sign=sign} = formatOrient3 line
	    val sign' = minSphere3 ((ax, ay, az), (bx, by, bz), (cx, cy, cz))(px, py, pz)
	in
	    sign = sign'
	end


    fun checkIntervalMinSphere3 line = 
	let val {num=i, a=(ax, ay, az), b=(bx, by, bz), c=(cx, cy, cz), 
		 d=(px, py, pz), sign=sign} = formatOrient3 line
	    val sign' = intervalMinSphere3 ((ax, ay, az), (bx, by, bz), (cx, cy, cz))(px, py, pz)
	in
	    sign = sign'
	end


    fun checkExfloatMinSphere3 line = 
	let val {num=i, a=(ax, ay, az), b=(bx, by, bz), c=(cx, cy, cz), 
		 d=(px, py, pz), sign=sign} = formatOrient3 line
	    val sign' = exfloatMinSphere3 ((ax, ay, az), (bx, by, bz), (cx, cy, cz))(px, py, pz)
	in
	    sign = sign'
	end
    

    fun checkMinSphere3 line = 
	let val {num=i, a=(ax, ay, az), b=(bx, by, bz), c=(cx, cy, cz), 
		 d=(px, py, pz), sign=sign} = formatOrient3 line

	    val r = ExactPredicates.smlStagedMinSphere3 
		[ax, ay, az, bx, by, bz, cx, cy, cz] [px, py, pz] 
	in
	    r = sign
	end






    (******************************************************
     ** FLOATING POINT IMPLEMENTATIONS
     ******************************************************)


    fun floatOrient3 [ ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz ] =
	let val bcx = bx - cx 
	    val acx = ax - cx 
	    val bcy = by - cy 
	    val acy = ay - cy 
	    val bcz = bz - cz 
	    val acz = az - cz 
	    val xMinor = ( bcy * acz ) - ( acy * bcz ) 
	    val yMinor = ( bcz * acx ) - ( acz * bcx ) 
	    val zMinor = ( bcx * acy ) - ( acx * bcy ) 
		
	    val dcx = dx - cx 
	    val dcy = dy - cy 
	    val dcz = dz - cz 
	    val d = dcx * xMinor + dcy * yMinor + dcz * zMinor 
	in
	    Real.sign d
	end


    fun checkFloatOrient3 line =
	let val {num=i,a=(ax, ay, az),b=(bx, by, bz),
		 c=(cx, cy, cz),d=(dx, dy, dz),sign=sign} = formatOrient3 line
	    val r = floatOrient3 [ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz]
	in
	    r = r
	end


    fun floatMinSphere3 [ ax, ay, az, bx, by, bz, cx, cy, cz ] =
	let val abx = bx - ax 
	    val aby = by - ay 
	    val abz = bz - az 
	    val acx = cx - ax 
	    val acy = cy - ay 
	    val acz = cz - az 
		
	    val ab = abx*abx + aby*aby + abz*abz 
	    val ac = acx*acx + acy*acy + acz*acz 
	    val bc = abx * acx + aby * acy + abz * acz 
		
	    val T = ab * ac 
	    val detA = T - bc * bc
	    val detB = T - ac * bc 
	    val detC = T - ab * bc 
	
	    val ccx = ax * detA + abx * detB + acx * detC 
	    val ccy = ay * detA + aby * detB + acy * detC 
	    val ccz = az * detA + abz * detB + acz * detC 
	in
	    fn [ px, py, pz ] =>  
	    let val apx = px - ax 
		val apy = py - ay 
		val apz = pz - az 
		    
		val d = apx * ( ccx - detA * px ) + apy * ( ccy - detA * py ) + apz * ( ccz - detA * pz ) 
	    in
		Real.sign d
	    end 
	end 




    fun checkFloatMinSphere3 line = 
	let val {num=i, a=(ax, ay, az), b=(bx, by, bz), c=(cx, cy, cz), 
		 d=(px, py, pz), sign=sign} = formatOrient3 line

	    val r = floatMinSphere3 [ax, ay, az, bx, by, bz, cx, cy, cz] [px, py, pz] 
	in
	    r = r
	end





    fun floatOrient2 [ ax, ay, bx, by, cx, cy ] =
	let val abx = ax - bx  
	    val aby = ay - by 
		
	    val d = aby * ( cx - bx ) + abx * ( by - cy ) 
	in
	    Real.sign d
	end



    fun checkFloatOrient2 line =
	let val {num=i, a=(ax, ay), b=(bx, by), 
		 c=(cx, cy), sign=sign} = formatOrient2 line
	    val r = floatOrient2 [ax, ay, bx, by, cx, cy]
	in
	    r = r
	end



    
    fun floatInSphere2 [ ax, ay, bx, by, cx, cy, dx, dy ] =
	let val bcx = bx - cx 
	    val acx = ax - cx 
	    val bcy = by - cy 
	    val acy = ay - cy 
	    val acSquare = ( acx * acx ) + ( acy * acy ) 
	    val bcSquare = ( bcx * bcx ) + ( bcy * bcy ) 
	    val xMinor = ( bcy * acSquare ) - ( acy * bcSquare ) 
	    val yMinor = ( bcSquare * acx ) - ( acSquare * bcx ) 
	    val zMinor = ( bcx * acy ) - ( acx * bcy ) 
		
	    val dcx = dx - cx 
	    val dcy = dy - cy 
	    val dcSquare = ( dcx * dcx ) + ( dcy * dcy ) 
	    val d = dcx * xMinor + dcy * yMinor + dcSquare * zMinor 
	in
	    Real.sign d
	end 



    fun checkFloatInSphere2 line =
	let val {num=i,a=(ax, ay),b=(bx, by),
		 c=(cx, cy),d=(dx, dy),sign=sign} = formatInSphere2 line
	    val r = floatInSphere2 [ax, ay, bx, by, cx, cy, dx, dy]
	in
	    r = r
	end





    fun floatStagedOrient3 [ ax, ay, az, bx, by, bz, cx, cy, cz ] =
	let val bcx = bx - cx 
	    val acx = ax - cx 
	    val bcy = by - cy 
	    val acy = ay - cy 
	    val bcz = bz - cz 
	    val acz = az - cz 
	    val xMinor = ( bcy * acz ) - ( acy * bcz ) 
	    val yMinor = ( bcz * acx ) - ( acz * bcx ) 
	    val zMinor = ( bcx * acy ) - ( acx * bcy ) 
	in
	    fn [ dx, dy, dz ] =>
	    let val dcx = dx - cx 
		val dcy = dy - cy 
		val dcz = dz - cz 
		val d = dcx * xMinor + dcy * yMinor + dcz * zMinor 
	    in
		Real.sign d
	    end
	end


    
    fun checkFloatStagedOrient3 line =
	let val {num=i,a=(ax, ay, az),b=(bx, by, bz),
		 c=(cx, cy, cz),d=(dx, dy, dz),sign=sign} = formatOrient3 line
	    val r = floatStagedOrient3 [ax, ay, az, bx, by, bz, cx, cy, cz] [dx, dy, dz]
	in
	    r = r
	end




    fun floatInSphere3 [ ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz, ex, ey, ez ] =
	let val adx = ax - dx 
	    val bdx = bx - dx  
	    val cdx = cx - dx 
	    val ady = ay - dy 
	    val bdy = by - dy 
	    val cdy = cy - dy 
	    val adz = az - dz 
	    val bdz = bz - dz 
	    val cdz = cz - dz 
	    val adSquare = ( adx * adx ) + ( ady * ady ) + ( adz * adz ) 
	    val bdSquare = ( bdx * bdx ) + ( bdy * bdy ) + ( bdz * bdz ) 
	    val cdSquare = ( cdx * cdx ) + ( cdy * cdy ) + ( cdz * cdz ) 
	    val abxy = ( adx * bdy ) - ( bdx * ady ) 
	    val abyz = ( ady * bdz ) - ( bdy * adz ) 
	    val abxz = ( adx * bdz ) - ( bdx * adz ) 
	    val bcxy = ( bdx * cdy ) - ( cdx * bdy ) 
	    val bcyz = ( bdy * cdz ) - ( cdy * bdz ) 
	    val bcxz = ( bdx * cdz ) - ( cdx * bdz ) 
	    val caxy = ( cdx * ady ) - ( adx * cdy ) 
	    val cayz = ( cdy * adz ) - ( ady * cdz ) 
	    val caxz = ( cdx * adz ) - ( adx * cdz ) 
	    val xMinor = bcyz * adSquare + cayz * bdSquare + abyz * cdSquare 
	    val yMinor = bcxz * adSquare + caxz * bdSquare + abxz * cdSquare 
	    val zMinor = bcxy * adSquare + caxy * bdSquare + abxy * cdSquare 
	    val uMinor = adx * bcyz + bdx * cayz + cdx * abyz 
	    val edx = ex - dx 
	    val edy = ey - dy 
	    val edz = ez - dz 
	    val edSquare = ( edx * edx ) + ( edy * edy ) + ( edz * edz ) 
	    val d = ( edx * xMinor - edy * yMinor ) + ( edz * zMinor - edSquare * uMinor ) 
	in
	    Real.sign d
	end
    



    fun checkFloatInSphere3 line =
	let val {num=i,a=(ax, ay, az),b=(bx, by, bz),
		 c=(cx, cy, cz),d=(dx, dy, dz),e=(ex, ey, ez),sign=sign} = formatInSphere3 line
	    val r = floatInSphere3 [ax, ay, az, bx, by, bz, cx, cy, cz, 
				    dx, dy, dz, ex, ey, ez]
	in
	    r = r
	end





    fun floatStagedInSphere3 [ ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz ] =
	let val adx = ax - dx 
	    val bdx = bx - dx  
	    val cdx = cx - dx 
	    val ady = ay - dy 
	    val bdy = by - dy 
	    val cdy = cy - dy 
	    val adz = az - dz 
	    val bdz = bz - dz 
	    val cdz = cz - dz 
	    val adSquare = ( adx * adx ) + ( ady * ady ) + ( adz * adz ) 
	    val bdSquare = ( bdx * bdx ) + ( bdy * bdy ) + ( bdz * bdz ) 
	    val cdSquare = ( cdx * cdx ) + ( cdy * cdy ) + ( cdz * cdz ) 
	    val abxy = ( adx * bdy ) - ( bdx * ady ) 
	    val abyz = ( ady * bdz ) - ( bdy * adz ) 
	    val abxz = ( adx * bdz ) - ( bdx * adz ) 
	    val bcxy = ( bdx * cdy ) - ( cdx * bdy ) 
	    val bcyz = ( bdy * cdz ) - ( cdy * bdz ) 
	    val bcxz = ( bdx * cdz ) - ( cdx * bdz ) 
	    val caxy = ( cdx * ady ) - ( adx * cdy ) 
	    val cayz = ( cdy * adz ) - ( ady * cdz ) 
	    val caxz = ( cdx * adz ) - ( adx * cdz ) 
	    val xMinor = bcyz * adSquare + cayz * bdSquare + abyz * cdSquare 
	    val yMinor = bcxz * adSquare + caxz * bdSquare + abxz * cdSquare 
	    val zMinor = bcxy * adSquare + caxy * bdSquare + abxy * cdSquare 
	    val uMinor = adx * bcyz + bdx * cayz + cdx * abyz 
	in
	    fn [ ex, ey, ez ] =>
	    let val edx = ex - dx 
		val edy = ey - dy 
		val edz = ez - dz 
		val edSquare = ( edx * edx ) + ( edy * edy ) + ( edz * edz ) 
		val d = ( edx * xMinor - edy * yMinor ) + ( edz * zMinor - edSquare * uMinor ) 
	    in
		Real.sign d
	    end
	end





    fun checkFloatStagedInSphere3 line =
	let val {num=i,a=(ax, ay, az),b=(bx, by, bz),
		 c=(cx, cy, cz),d=(dx, dy, dz),e=(ex, ey, ez),sign=sign} = formatInSphere3 line
	    val r = floatStagedInSphere3 [ax, ay, az, bx, by, bz, cx, cy, cz, 
							dx, dy, dz] [ex, ey, ez]
	in
	    r = r
	end




    (******************************************************
     ** INTERVAL IMPLEMENTATIONS + EXFLOAT EXACT PHASE
     ******************************************************)


    fun intervalOrient3 [ ax', ay', az', bx', by', bz', cx', cy', cz', dx', dy', dz' ] =
	let open I
	    val [ ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz ] =
		map fromNumber [ ax', ay', az', bx', by', bz', cx', cy', cz', dx', dy', dz' ] 
	    val bcx = bx - cx 
	    val acx = ax - cx 
	    val bcy = by - cy 
	    val acy = ay - cy 
	    val bcz = bz - cz 
	    val acz = az - cz 
	    val xMinor = ( bcy * acz ) - ( acy * bcz ) 
	    val yMinor = ( bcz * acx ) - ( acz * bcx ) 
	    val zMinor = ( bcx * acy ) - ( acx * bcy ) 
		
	    val dcx = dx - cx 
	    val dcy = dy - cy 
	    val dcz = dz - cz 
	    val d = dcx * xMinor + dcy * yMinor + dcz * zMinor 
	in
	    if d > zero then 1
	    else if d < zero then ~1
		 else
		     let open QX
			 val bcx = diff (bx', cx')
			 val acx = diff (ax', cx')
			 val bcy = diff (by', cy')
			 val acy = diff (ay', cy')
			 val bcz = diff (bz', cz')
			 val acz = diff (az', cz')
			 val xMinor = X.-( bcy * acz, acy * bcz ) 
			 val yMinor = X.-( bcz * acx, acz * bcx ) 
			 val zMinor = X.-( bcx * acy, acx * bcy ) 
			     
			 val dcx = diff (dx', cx')
			 val dcy = diff (dy', cy')
			 val dcz = diff (dz', cz')
			 open X
			 val d = scale2(dcx, xMinor) + scale2(dcy, yMinor) + scale2(dcz, zMinor)
		     in
			 sign d
		     end
	end


    fun checkIntervalOrient3 line =
	let val {num=i,a=(ax, ay, az),b=(bx, by, bz),
		 c=(cx, cy, cz),d=(dx, dy, dz),sign=sign} = formatOrient3 line
	    val r = intervalOrient3 [ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz]
	in
	    r = sign
	end

(*

    fun floatMinSphere3 [ ax, ay, az, bx, by, bz, cx, cy, cz ] =
	let val abx = bx - ax 
	    val aby = by - ay 
	    val abz = bz - az 
	    val acx = cx - ax 
	    val acy = cy - ay 
	    val acz = cz - az 
		
	    val ab = abx*abx + aby*aby + abz*abz 
	    val ac = acx*acx + acy*acy + acz*acz 
	    val bc = abx * acx + aby * acy + abz * acz 
		
	    val T = ab * ac 
	    val detA = T - bc * bc
	    val detB = T - ac * bc 
	    val detC = T - ab * bc 
	
	    val ccx = ax * detA + abx * detB + acx * detC 
	    val ccy = ay * detA + aby * detB + acy * detC 
	    val ccz = az * detA + abz * detB + acz * detC 
	in
	    fn [ px, py, pz ] =>  
	    let val apx = px - ax 
		val apy = py - ay 
		val apz = pz - az 
		    
		val d = apx * ( ccx - detA * px ) + apy * ( ccy - detA * py ) + apz * ( ccz - detA * pz ) 
	    in
		Real.sign d
	    end 
	end 




    fun checkFloatMinSphere3 line = 
	let val {num=i, a=(ax, ay, az), b=(bx, by, bz), c=(cx, cy, cz), 
		 d=(px, py, pz), sign=sign} = formatOrient3 line

	    val r = floatMinSphere3 [ax, ay, az, bx, by, bz, cx, cy, cz] [px, py, pz] 
	in
	    r = r
	end





    fun floatOrient2 [ ax, ay, bx, by, cx, cy ] =
	let val abx = ax - bx  
	    val aby = ay - by 
		
	    val d = aby * ( cx - bx ) + abx * ( by - cy ) 
	in
	    Real.sign d
	end



    fun checkFloatOrient2 line =
	let val {num=i, a=(ax, ay), b=(bx, by), 
		 c=(cx, cy), sign=sign} = formatOrient2 line
	    val r = floatOrient2 [ax, ay, bx, by, cx, cy]
	in
	    r = r
	end



    
    fun floatInSphere2 [ ax, ay, bx, by, cx, cy, dx, dy ] =
	let val bcx = bx - cx 
	    val acx = ax - cx 
	    val bcy = by - cy 
	    val acy = ay - cy 
	    val acSquare = ( acx * acx ) + ( acy * acy ) 
	    val bcSquare = ( bcx * bcx ) + ( bcy * bcy ) 
	    val xMinor = ( bcy * acSquare ) - ( acy * bcSquare ) 
	    val yMinor = ( bcSquare * acx ) - ( acSquare * bcx ) 
	    val zMinor = ( bcx * acy ) - ( acx * bcy ) 
		
	    val dcx = dx - cx 
	    val dcy = dy - cy 
	    val dcSquare = ( dcx * dcx ) + ( dcy * dcy ) 
	    val d = dcx * xMinor + dcy * yMinor + dcSquare * zMinor 
	in
	    Real.sign d
	end 



    fun checkFloatInSphere2 line =
	let val {num=i,a=(ax, ay),b=(bx, by),
		 c=(cx, cy),d=(dx, dy),sign=sign} = formatInSphere2 line
	    val r = floatInSphere2 [ax, ay, bx, by, cx, cy, dx, dy]
	in
	    r = r
	end





    fun floatStagedOrient3 [ ax, ay, az, bx, by, bz, cx, cy, cz ] =
	let val bcx = bx - cx 
	    val acx = ax - cx 
	    val bcy = by - cy 
	    val acy = ay - cy 
	    val bcz = bz - cz 
	    val acz = az - cz 
	    val xMinor = ( bcy * acz ) - ( acy * bcz ) 
	    val yMinor = ( bcz * acx ) - ( acz * bcx ) 
	    val zMinor = ( bcx * acy ) - ( acx * bcy ) 
	in
	    fn [ dx, dy, dz ] =>
	    let val dcx = dx - cx 
		val dcy = dy - cy 
		val dcz = dz - cz 
		val d = dcx * xMinor + dcy * yMinor + dcz * zMinor 
	    in
		Real.sign d
	    end
	end


    
    fun checkFloatStagedOrient3 line =
	let val {num=i,a=(ax, ay, az),b=(bx, by, bz),
		 c=(cx, cy, cz),d=(dx, dy, dz),sign=sign} = formatOrient3 line
	    val r = floatStagedOrient3 [ax, ay, az, bx, by, bz, cx, cy, cz] [dx, dy, dz]
	in
	    r = r
	end




    fun floatInSphere3 [ ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz, ex, ey, ez ] =
	let val adx = ax - dx 
	    val bdx = bx - dx  
	    val cdx = cx - dx 
	    val ady = ay - dy 
	    val bdy = by - dy 
	    val cdy = cy - dy 
	    val adz = az - dz 
	    val bdz = bz - dz 
	    val cdz = cz - dz 
	    val adSquare = ( adx * adx ) + ( ady * ady ) + ( adz * adz ) 
	    val bdSquare = ( bdx * bdx ) + ( bdy * bdy ) + ( bdz * bdz ) 
	    val cdSquare = ( cdx * cdx ) + ( cdy * cdy ) + ( cdz * cdz ) 
	    val abxy = ( adx * bdy ) - ( bdx * ady ) 
	    val abyz = ( ady * bdz ) - ( bdy * adz ) 
	    val abxz = ( adx * bdz ) - ( bdx * adz ) 
	    val bcxy = ( bdx * cdy ) - ( cdx * bdy ) 
	    val bcyz = ( bdy * cdz ) - ( cdy * bdz ) 
	    val bcxz = ( bdx * cdz ) - ( cdx * bdz ) 
	    val caxy = ( cdx * ady ) - ( adx * cdy ) 
	    val cayz = ( cdy * adz ) - ( ady * cdz ) 
	    val caxz = ( cdx * adz ) - ( adx * cdz ) 
	    val xMinor = bcyz * adSquare + cayz * bdSquare + abyz * cdSquare 
	    val yMinor = bcxz * adSquare + caxz * bdSquare + abxz * cdSquare 
	    val zMinor = bcxy * adSquare + caxy * bdSquare + abxy * cdSquare 
	    val uMinor = adx * bcyz + bdx * cayz + cdx * abyz 
	    val edx = ex - dx 
	    val edy = ey - dy 
	    val edz = ez - dz 
	    val edSquare = ( edx * edx ) + ( edy * edy ) + ( edz * edz ) 
	    val d = ( edx * xMinor - edy * yMinor ) + ( edz * zMinor - edSquare * uMinor ) 
	in
	    Real.sign d
	end
    



    fun checkFloatInSphere3 line =
	let val {num=i,a=(ax, ay, az),b=(bx, by, bz),
		 c=(cx, cy, cz),d=(dx, dy, dz),e=(ex, ey, ez),sign=sign} = formatInSphere3 line
	    val r = floatInSphere3 [ax, ay, az, bx, by, bz, cx, cy, cz, 
				    dx, dy, dz, ex, ey, ez]
	in
	    r = r
	end





    fun floatStagedInSphere3 [ ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz ] =
	let val adx = ax - dx 
	    val bdx = bx - dx  
	    val cdx = cx - dx 
	    val ady = ay - dy 
	    val bdy = by - dy 
	    val cdy = cy - dy 
	    val adz = az - dz 
	    val bdz = bz - dz 
	    val cdz = cz - dz 
	    val adSquare = ( adx * adx ) + ( ady * ady ) + ( adz * adz ) 
	    val bdSquare = ( bdx * bdx ) + ( bdy * bdy ) + ( bdz * bdz ) 
	    val cdSquare = ( cdx * cdx ) + ( cdy * cdy ) + ( cdz * cdz ) 
	    val abxy = ( adx * bdy ) - ( bdx * ady ) 
	    val abyz = ( ady * bdz ) - ( bdy * adz ) 
	    val abxz = ( adx * bdz ) - ( bdx * adz ) 
	    val bcxy = ( bdx * cdy ) - ( cdx * bdy ) 
	    val bcyz = ( bdy * cdz ) - ( cdy * bdz ) 
	    val bcxz = ( bdx * cdz ) - ( cdx * bdz ) 
	    val caxy = ( cdx * ady ) - ( adx * cdy ) 
	    val cayz = ( cdy * adz ) - ( ady * cdz ) 
	    val caxz = ( cdx * adz ) - ( adx * cdz ) 
	    val xMinor = bcyz * adSquare + cayz * bdSquare + abyz * cdSquare 
	    val yMinor = bcxz * adSquare + caxz * bdSquare + abxz * cdSquare 
	    val zMinor = bcxy * adSquare + caxy * bdSquare + abxy * cdSquare 
	    val uMinor = adx * bcyz + bdx * cayz + cdx * abyz 
	in
	    fn [ ex, ey, ez ] =>
	    let val edx = ex - dx 
		val edy = ey - dy 
		val edz = ez - dz 
		val edSquare = ( edx * edx ) + ( edy * edy ) + ( edz * edz ) 
		val d = ( edx * xMinor - edy * yMinor ) + ( edz * zMinor - edSquare * uMinor ) 
	    in
		Real.sign d
	    end
	end





    fun checkFloatStagedInSphere3 line =
	let val {num=i,a=(ax, ay, az),b=(bx, by, bz),
		 c=(cx, cy, cz),d=(dx, dy, dz),e=(ex, ey, ez),sign=sign} = formatInSphere3 line
	    val r = floatStagedInSphere3 [ax, ay, az, bx, by, bz, cx, cy, cz, 
							dx, dy, dz] [ex, ey, ez]
	in
	    r = r
	end
*)




 

      (****************************************************
       ** TIMING OFFSETS 
       ****************************************************)


    fun void l = 1
    fun void2 l1 l2 = 1

    fun timeOrient3 line =
	let val {num=i,a=(ax, ay, az),b=(bx, by, bz),
		 c=(cx, cy, cz),d=(dx, dy, dz),sign=sign} = formatOrient3 line
	    val r = void [ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz]
	in
	    r = r
	end

    fun timeMinSphere3 line =
	let val {num=i,a=(ax, ay, az),b=(bx, by, bz),
		 c=(cx, cy, cz),d=(dx, dy, dz),sign=sign} = formatOrient3 line
	    val r = void [ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz]
	in
	    r = r
	end

    fun timeOrient2 line =
	let val {num=i, a=(ax, ay), b=(bx, by), 
		 c=(cx, cy), sign=sign} = formatOrient2 line
	    val r = void [ax, ay, bx, by, cx, cy]
	in
	    r = r
	end

    
    fun timeInSphere2 line =
	let val {num=i,a=(ax, ay),b=(bx, by),
		 c=(cx, cy),d=(dx, dy),sign=sign} = formatInSphere2 line
	    val r = void [ax, ay, bx, by, cx, cy, dx, dy]
	in
	    r = r
	end


    fun timeStagedOrient3 line =
	let val {num=i,a=(ax, ay, az),b=(bx, by, bz),
		 c=(cx, cy, cz),d=(dx, dy, dz),sign=sign} = formatOrient3 line
	    val r = void2 [ax, ay, az, bx, by, bz, cx, cy, cz] [dx, dy, dz]
	in
	    r = r
	end


    fun timeInSphere3 line =
	let val {num=i,a=(ax, ay, az),b=(bx, by, bz),
		 c=(cx, cy, cz),d=(dx, dy, dz),e=(ex, ey, ez),sign=sign} = formatInSphere3 line
	    val r = void [ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz, ex, ey, ez]
	in
	    r = r
	end


    fun timeStagedInSphere3 line =
	let val {num=i,a=(ax, ay, az),b=(bx, by, bz),
		 c=(cx, cy, cz),d=(dx, dy, dz),e=(ex, ey, ez),sign=sign} = formatInSphere3 line
	    val r = void2 [ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz] [ex, ey, ez]
	in
	    r = r
	end




    fun loop10 f file =
	let fun loop' 0 = true
	      | loop' n = 
		let val infile = TextIO.openIn file
		in
		    ((go f infile) before (TextIO.closeIn infile))
		    andalso (loop' (n-1))
		end
	in
	    loop' 10
	end
	

    fun loop10 f file =
	let val infile = TextIO.openIn file
	    fun loop' 0 = true
	      | loop' n = 
		let val infile = TextIO.openIn file
		in
		    ((go f infile) before (TextIO.closeIn infile))
		    andalso (loop' (n-1))
		end
	in
	    loop' 10
	end



    fun time (offsetTiming, checkTiming, file) =
	let val _ = print "Computing Offset\n"
	    val offsetTimer = Timer.startCPUTimer()
	    val _ = loop10 offsetTiming file
	    val offsetTime = #usr (Timer.checkCPUTimer offsetTimer)

	    val _ = print "Computing Predicate Time\n"
	    val userTimer = Timer.startCPUTimer()
	    val true = loop10 checkTiming file
	    val userTime = #usr (Timer.checkCPUTimer userTimer)

	    val time = Time.toMilliseconds (Time.-(userTime, offsetTime))
	in
	    time 
	end


    fun dummy l = true




    (*************************************************
     ** MAIN FUNCTIONS
     *************************************************)
 

    fun test' 0 = time (timeOrient2, checkFloatOrient2, "orient.2d")
      |	test' 1 = time (timeOrient2, checkOrient2, "orient.2d")

      | test' 2 = time (timeInSphere2, checkFloatInSphere2, "insphere.2d")
      | test' 3 = time (timeInSphere2, checkInSphere2, "insphere.2d")

      | test' 4 = time (timeOrient3, checkFloatOrient3, "orient.3d")
      | test' 5 = 
	let val _ = IEEEReal.setRoundingMode (IEEEReal.TO_NEGINF)
	    val t = time (timeOrient3, checkIntervalOrient3, "orient.3d")
	    val _ = IEEEReal.setRoundingMode (IEEEReal.TO_NEAREST)
	in
	    t
	end 
      | test' 6 = time (timeOrient3, checkOrient3, "orient.3d")

      | test' 7 = time (timeInSphere3, checkFloatInSphere3, "insphere.3d")
      | test' 8 = time (timeInSphere3, checkInSphere3, "insphere.3d")

      | test' 9 = time (timeInSphere3, checkFloatInSphere3, "insphere.3d")
      | test' 10 = time (timeInSphere3, checkInSphere3'', "insphere.3d")

      | test' 11 = time (timeStagedOrient3, checkFloatStagedOrient3, "orient.3d")
      | test' 12 = time (timeStagedOrient3, checkStagedOrient3, "orient.3d")

      | test' 13 = time (timeStagedInSphere3, checkFloatStagedInSphere3, "insphere.3d")
      | test' 14 = time (timeStagedInSphere3, checkStagedInSphere3, "insphere.3d")

      | test' 15 = time (timeMinSphere3, checkFloatMinSphere3, "minsphere.3d")
      | test' 16 = 0
(*	let val _ = IEEEReal.setRoundingMode (IEEEReal.TO_NEGINF)
	    val t = time (timeMinSphere3, checkRationalMinSphere3, "minsphere.3d")
	    val _ = IEEEReal.setRoundingMode (IEEEReal.TO_NEAREST)
	in
	    t
	end *)
      | test' 17 = 
	let val _ = IEEEReal.setRoundingMode (IEEEReal.TO_NEGINF)
	    val t = time (timeMinSphere3, checkIntervalMinSphere3, "minsphere.3d")
	    val _ = IEEEReal.setRoundingMode (IEEEReal.TO_NEAREST)
	in
	    t
	end 
      | test' 18 = time (timeMinSphere3, checkMinSphere3, "minsphere.3d")
      | test' 19 = 
	let val _ = IEEEReal.setRoundingMode (IEEEReal.TO_NEGINF)
	    val t = time (timeMinSphere3, checkExfloatMinSphere3, "minsphere.3d")
	    val _ = IEEEReal.setRoundingMode (IEEEReal.TO_NEAREST)
	in
	    t
	end


    fun report msg time = 
	true before
	print (msg^(LargeInt.toString time)^"ms\n")


    fun test i =
	if (i < 0) orelse (i > lastTest) then raise Nonexistent
	else
	    report ("Test "^(Int.toString i)^". ") (test' i)


    fun testAll () = 
	let fun check' i = 
	    if i > lastTest then true
	    else (test i) andalso (check' (i+1))
	in
	    check' 0
	end
end
