(***************************************
 ** CompilerTest.sml
 ** sml
 ** 
 ** Aleksandar Nanevski
 **
 ** Test file for the compiler
 ***************************************)

structure CompilerTest :> TEST =
struct
    exception Nonexistent

    val lastTest = 15

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
	    (* print for debugging purposes *)
	    (* val _ = print ((Int.toString i) ^ " " ^ 
			   (Int.toString sign) ^ " " ^ (Int.toString r) ^ "\n")  *)
	in
	    r = ~ sign
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




    fun RationalInSphere2 [ax, ay, bx, by, cx, cy, dx, dy] =
	let open RoundDownLazyRational 
	    val [ax, ay, bx, by, cx, cy, dx, dy] =
		map fromReal [ax, ay, bx, by, cx, cy, dx, dy]
	    val (adx, ady) = (ax - dx, ay - dy)
	    val (bdx, bdy) = (bx - dx, by - dy)
	    val (cdx, cdy) = (cx - dx, cy - dy)
		
	    val (bdxcdy, cdxbdy, alift) = (bdx * cdy, cdx * bdy, adx*adx + ady*ady)
	    val (cdxady, adxcdy, blift) = (cdx * ady, adx * cdy, bdx*bdx + bdy*bdy)
	    val (adxbdy, bdxady, clift) = (adx * bdy, bdx * ady, cdx*cdx + cdy*cdy)
		
	    val det = alift * (bdxcdy - cdxbdy) 
		+ blift * (cdxady - adxcdy)
		+ clift * (adxbdy - bdxady)
	in
	    sign det
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


		

    fun checkRationalMinSphere3 line = 
	let val {num=i, a=(ax, ay, az), b=(bx, by, bz), c=(cx, cy, cz), 
		 d=(px, py, pz), sign=sign} = formatOrient3 line
	    val sign' = minSphere3 ((ax, ay, az), (bx, by, bz), (cx, cy, cz))(px, py, pz)
	in
	    sign = sign'
	end

    fun checkIntervalOrient3 line = 
	let val {num=i, a=(ax, ay, az), b=(bx, by, bz), c=(cx, cy, cz), 
		 d=(px, py, pz), sign=sign} = formatOrient3 line
	    val sign' = intervalOrient3 [ax, ay, az, bx, by, bz, cx, cy, cz, px, py, pz]
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



    fun test' 0 =
	let val (points as [ax, ay, bx, by, cx, cy, dx, dy]) = 
	    [~1.4709981102017870e15, ~1.5818562552348894e03, 
	     ~4.9329978308411263e~4, ~2.3436889280960234e05,
	      1.3666719604983426e22,  1.0706247728503862e04,
	     ~1.3384603448702672e06,  3.7349397119125453e32]
	    val s = ExactPredicates.smlInSphere2 points
	in
	    s = ~1 
	end
      | test' 1 = 
	let val (points as [ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz, ex, ey, ez]) =
	    [~2.18039355867E25, 4.79320454566E24, 19622.3940581, 
	     ~8.5431760978E14, 1.64166472669E12, ~595634080051.0, 
	     2117108824.23, 2.53188033161E33, ~2.52680485007E19, 
	     2.32448318724E21, 6.76947059798E30, 3.29470792528E26, 
	     1064332823.26, 0.0103128698085, 0.0213719429183]
	    val s = ExactPredicates.smlInSphere3 points
	in
	    s = 1
	end
      | test' 2 = 
	let val infile = TextIO.openIn "orient.2d"
	in
	    (go checkOrient2 infile)
	    before (TextIO.closeIn infile)
	end 
      | test' 3 = 
	let val infile = TextIO.openIn "insphere.2d"
	in
	    (go checkInSphere2 infile)
	    before (TextIO.closeIn infile)
	end 
      | test' 4 =
	let val infile = TextIO.openIn "orient.3d"
	in
	    (go checkOrient3 infile)
	    before (TextIO.closeIn infile)
	end 
      | test' 5 = 
	let val infile = TextIO.openIn "insphere.3d"
	in
	    (go checkInSphere3 infile)
	    before (TextIO.closeIn infile)
	end 
      | test' 6 =
	let val infile = TextIO.openIn "orient.3d"
	in
	    (go checkStagedOrient3 infile)
	    before (TextIO.closeIn infile)
	end
      | test' 7 = 
	let val infile = TextIO.openIn "insphere.3d"
	in
	    (go checkStagedInSphere3 infile)
	    before (TextIO.closeIn infile)
	end 
      | test' 8 =
	let val [ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz] =
	    [0.0, 0.0, 0.0,  3.0, 0.0, 0.0,  0.0, 4.0, 0.0,  1.5, 2.0, 2.5]
	    val s = ExactPredicates.smlStagedMinSphere3 
		      [ax, ay, az, bx, by, bz, cx, cy, cz] [dx, dy, dz]
	in
	    s = 0
	end
      | test' 9 = 
	let val infile = TextIO.openIn "minsphere.3d"
	    val _ = IEEEReal.setRoundingMode (IEEEReal.TO_NEGINF)
	in
	    (go checkRationalMinSphere3 infile)
	    before ((TextIO.closeIn infile);
		    IEEEReal.setRoundingMode (IEEEReal.TO_NEAREST))
	end 
      | test' 10 = 
	let val infile = TextIO.openIn "minsphere.3d"
	    val _ = IEEEReal.setRoundingMode (IEEEReal.TO_NEGINF)
	in
	    (go checkIntervalMinSphere3 infile)
	    before ((TextIO.closeIn infile);
		    IEEEReal.setRoundingMode (IEEEReal.TO_NEAREST))
	end 
      | test' 11 = 
	let val infile = TextIO.openIn "minsphere.3d"
	in
	    (go checkMinSphere3 infile)
	    before (TextIO.closeIn infile)
	end 
      | test' 12 = 
	let val infile = TextIO.openIn "minsphere.3d"
	    val _ = IEEEReal.setRoundingMode (IEEEReal.TO_NEGINF)
	in
	    (go checkExfloatMinSphere3 infile)
	    before ((TextIO.closeIn infile);
		    IEEEReal.setRoundingMode (IEEEReal.TO_NEAREST))
	end 
      | test' 13 = 
	let val infile = TextIO.openIn "insphere.3d"
	in
	    (go checkInSphere3'' infile)
	    before (TextIO.closeIn infile)
	end 
      | test' 14 = 
	let val infile = TextIO.openIn "orient.3d"
	    val _ = IEEEReal.setRoundingMode (IEEEReal.TO_NEGINF)
	in
	    (go checkIntervalOrient3 infile)
	    before ((TextIO.closeIn infile);
		    IEEEReal.setRoundingMode (IEEEReal.TO_NEAREST))
	end 
      | test' 15 = 
	let val _ = IEEEReal.setRoundingMode (IEEEReal.TO_NEGINF)
	    val s = RationalInSphere2 [~4.2000000000000002E00, 2.7000000000000000E01, 
				       ~4.5000000000000000E00, 2.8199999999999999E01, 
				       ~5.7000000000000002E00, 2.7899999999999999E01,
				       ~5.3999999999999986E00, 2.6699999999999999E01]
	    val _ = IEEEReal.setRoundingMode (IEEEReal.TO_NEAREST)
	    val p = ExactPredicates.smlInSphere2 [~4.2000000000000002E00, 2.7000000000000000E01, 
						  ~4.5000000000000000E00, 2.8199999999999999E01, 
						  ~5.7000000000000002E00, 2.7899999999999999E01,
						  ~5.3999999999999986E00, 2.6699999999999999E01]
	in
	    s = p
	end

              

    fun report msg flag = 
	flag before
	print (msg^(if flag then "OK\n" else "FAILED\n"))


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
