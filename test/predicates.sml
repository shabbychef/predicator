
  val insphere1to2 =
"  fn [ ax, ay, bx, by, px, py ] => " ^
"  let " ^
"    val vx = px - ax " ^
"    val vy = py - ay " ^
"    val wx = px - bx " ^
"    val wy = py - by " ^
"    val insphere = ( vx * wx ) + ( vy * wy ) " ^
"  end " 

  val insphere2to2 =
"  fn [ ax, ay, bx, by, cx, cy, px, py ] => " ^
"  let " ^
"    val brx = bx - ax " ^
"    val bry = by - ay " ^
"    val crx = cx - ax " ^
"    val cry = cy - ay " ^
"    val prx = px - ax " ^
"    val pry = py - ay " ^
"    val brSqrd = ( sq brx ) + ( sq bry ) " ^
"    val crSqrd = ( sq crx ) + ( sq cry ) " ^
"    val prSqrd = ( sq prx ) + ( sq pry )  " ^
"    val xMinor = ( bry * crSqrd ) - ( cry * brSqrd ) " ^
"    val yMinor = ( brx * crSqrd ) - ( crx * brSqrd )  " ^
"    val denom = ( brx * cry ) - ( crx * bry ) " ^
"    val numer = prx * xMinor - pry * yMinor + prSqrd * denom " ^
"    val insphere = numer * denom " ^
"  end " 

  val insphere1to3 = 
"  fn [ ax, ay, az, bx, by, bz, px, py, pz ] => " ^
"  let " ^
"    val vx = px - ax " ^
"    val vy = py - ay " ^
"    val vz = pz - az " ^
"    val wx = px - bx " ^
"    val wy = py - by " ^
"    val wz = pz - bz " ^
"    val insphere = ( vx * wx ) + ( vy * wy ) + ( vz * wz ) " ^
"  end " 

  val insphere2to3 = 
"  fn [ ax, ay, az, bx, by, bz, cx, cy, cz, px, py, pz ] => " ^
"  let " ^
"    val brx = bx - ax " ^
"    val bry = by - ay " ^
"    val brz = bz - az " ^
"    val crx = cx - ax " ^
"    val cry = cy - ay " ^
"    val crz = cz - az " ^
"    val prx = px - ax " ^
"    val pry = py - ay " ^
"    val prz = pz - az " ^
"    val v1v1 = ( sq brx ) + ( sq bry ) + ( sq brz ) " ^
"    val v2v2 = ( sq crx ) + ( sq cry ) + ( sq crz ) " ^
"    val ww = ( sq prx ) + ( sq pry ) + ( sq prz ) " ^
"    val v1v2 = ( brx * crx ) + ( bry * cry ) + ( brz * crz ) " ^
"    val wv1 = ( brx * prx ) + ( bry * pry ) + ( brz * prz ) " ^
"    val wv2 = ( prx * crx ) + ( pry * cry ) + ( prz * crz ) " ^
"    val xMinor = ( v1v2 * wv2 ) - ( wv1 * v2v2 ) " ^
"    val yMinor = ( v1v1 * wv2 ) - ( wv1 * v1v2 ) " ^
"    val denom = ( v1v1 * v2v2 ) - ( v1v2 * v1v2 )  " ^
"    val numer = ( v1v1 * xMinor ) - ( v2v2 * yMinor ) + ( ww * denom ) " ^
"    val insphere = numer * denom " ^
"  end " 

  val insphere3to3 = 
"  fn [ ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz, px, py, pz ] => " ^
"  let " ^
"    val brx = bx - ax " ^
"    val bry = by - ay " ^
"    val brz = bz - az " ^
"    val crx = cx - ax " ^
"    val cry = cy - ay " ^
"    val crz = cz - az " ^
"    val drx = dx - ax " ^
"    val dry = dy - ay " ^
"    val drz = dz - az " ^
"    val prx = px - ax " ^
"    val pry = py - ay " ^
"    val prz = pz - az " ^
"    val brSqrd = ( sq brx ) + ( sq bry ) + ( sq brz ) " ^
"    val crSqrd = ( sq crx ) + ( sq cry ) + ( sq crz ) " ^
"    val drSqrd = ( sq drx ) + ( sq dry ) + ( sq drz ) " ^
"    val prSqrd = ( sq prx ) + ( sq pry ) + ( sq prz ) " ^
"    val bxmin  = ( cry * drz ) - ( dry * crz ) " ^
"    val bymin  = ( crx * drz ) - ( drx * crz ) " ^
"    val bzmin  = ( crx * dry ) - ( drx * cry ) " ^
"    val cxmin  = ( bry * drz ) - ( dry * brz ) " ^
"    val cymin  = ( brx * drz ) - ( drx * brz ) " ^
"    val czmin  = ( brx * dry ) - ( drx * bry ) " ^
"    val dxmin  = ( bry * crz ) - ( cry * brz ) " ^
"    val dymin  = ( brx * crz ) - ( crx * brz ) " ^
"    val dzmin  = ( brx * cry ) - ( crx * bry ) " ^
"    val xMinor = ( bxmin * brSqrd ) - ( cxmin * crSqrd ) + ( dxmin * drSqrd ) " ^
"    val yMinor = ( bymin * brSqrd ) - ( cymin * crSqrd ) + ( dymin * drSqrd ) " ^
"    val zMinor = ( bzmin * brSqrd ) - ( czmin * crSqrd ) + ( dzmin * drSqrd ) " ^
"    val denom = ( brx * bxmin ) - ( crx * cxmin ) + ( drx * dxmin ) " ^
"    val numer = ( pry * yMinor - prx * xMinor ) + ( prSqrd * denom - prz * zMinor ) " ^
"    val insphere = numer * denom " ^
"  end " 

val is12 = Compile.generate insphere1to2 
val is22 = Compile.generate insphere2to2 
val is13 = Compile.generate insphere1to3 
val is23 = Compile.generate insphere2to3 
val is33 = Compile.generate insphere3to3 

(* val is12 = Compile.generateC insphere1to2  *)
(* val is22 = Compile.generateC insphere2to2  *)
(* val is13 = Compile.generateC insphere1to3  *)
(* val is23 = Compile.generateC insphere2to3  *)
(* val is33 = Compile.generateC insphere3to3  *)

val _ = print("val insphere1to2 =\n" ^ is12 ^ "\n\n")
val _ = print("val insphere2to2 =\n" ^ is22 ^ "\n\n")
val _ = print("val insphere1to3 =\n" ^ is13 ^ "\n\n")
val _ = print("val insphere2to3 =\n" ^ is23 ^ "\n\n")
val _ = print("val insphere3to3 =\n" ^ is33 ^ "\n\n")
