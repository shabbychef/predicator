(**************************************
 ** Context.sml
 ** sml
 ** 
 ** Aleksandar Nanevski
 **
 ** Contexts for error analysis and 
 ** compilation
 **************************************)


structure Context :> 
sig
    exception Error of string

    type context

    datatype error = 
	Oa of Ulps.rat | Ob of Ulps.rat | Oc of Ulps.rat*Ulps.rat*Ulps.rat | Od | P
	
    val empty : context

    val new : string * context -> Var.var * context
    val getVars : context -> Var.dict

    (* given a source var, create a target var for appropriate phase *)
    val newA : Var.var * context -> Var.var * context
    val newB : Var.var * context -> Var.var * context
    val newC : Var.var * context -> Var.var * context
    val newD : Var.var * context -> Var.var * context
    val newP : Var.var * context -> Var.var * context

    (* given a source var, find a target var for appropriate phase *)
    val getA : Var.var * context -> Var.var
    val getB : Var.var * context -> Var.var
    val getC : Var.var * context -> Var.var
    val getD : Var.var * context -> Var.var
    val getP : Var.var * context -> Var.var

    (* given set, select those vars from it that are in appropriate phase *)
    val domA : VarSet.set * context -> VarSet.set
    val domB : VarSet.set * context -> VarSet.set
    val domC : VarSet.set * context -> VarSet.set
    val domD : VarSet.set * context -> VarSet.set
    val domP : VarSet.set * context -> VarSet.set

    (* insert and retrieve errors and substitutions in context *)
    val putError : (Var.var * error) * context -> context
    val putMatch : (Var.var * error * Target.num) * context -> context
    val getError : Var.var * context -> error
    val getMatch : Var.var * context -> Target.num

    (* is the variable exact fp value? *)
    val free : Var.var * context -> bool
end =
struct
    exception Error of string

    structure Q = Ulps
    
    datatype error = Oa of Q.rat | Ob of Q.rat | Oc of Q.rat*Q.rat*Q.rat | Od | P
	
    type context = {errs : error VarTab.table, subst : Target.num VarTab.table,
		    matchTable : Var.var VarTab.table list, symbTable : Var.dict}
	
    val empty : context = {errs = VarTab.empty,  subst = VarTab.empty,
			   matchTable = [VarTab.empty, VarTab.empty, VarTab.empty, 
					 VarTab.empty, VarTab.empty],  
			   symbTable = Var.empty}

    fun getVars (c:context) = #symbTable c

    fun new (s, {errs, subst, matchTable, symbTable}) =
	let val (y, st) = Var.newBound (s, symbTable)
	in
	    (y, {errs=errs, subst=subst, 
		 matchTable=matchTable, symbTable=st})
	end


    fun newA (y, {errs, subst, matchTable=[As, Bs, Cs, Ds, Ps], symbTable}) = 
	let val (ya, st) = Var.newBound ((Var.name y), symbTable)
	    val As = VarTab.insert ((y, ya), As)
	in
	    (ya, {errs=errs, subst=subst, 
		  matchTable=[As,Bs,Cs,Ds,Ps], symbTable=st})
	end

    fun newB (y, {errs, subst, matchTable=[As, Bs, Cs, Ds, Ps], symbTable}) = 
	let val (yb, st) = Var.newBound ((Var.name y)^"B", symbTable)
	    val Bs = VarTab.insert ((y, yb), Bs)
	in
	    (yb, {errs=errs, subst=subst, 
		  matchTable=[As,Bs,Cs,Ds,Ps], symbTable=st})
	end
    
    fun newC (y, {errs, subst, matchTable=[As, Bs, Cs, Ds, Ps], symbTable}) = 
	let val (yc, st) = Var.newBound ((Var.name y)^"C", symbTable)
	    val Cs = VarTab.insert ((y, yc), Cs)
	in
	    (yc, {errs=errs, subst=subst, 
		  matchTable=[As,Bs,Cs,Ds,Ps], symbTable=st})
	end

    fun newD (y, {errs, subst, matchTable=[As, Bs, Cs, Ds, Ps], symbTable}) = 
	let val (yd, st) = Var.newBound ((Var.name y)^"D", symbTable)
	    val Ds = VarTab.insert ((y, yd), Ds)
	in
	    (yd, {errs=errs, subst=subst, 
		  matchTable=[As,Bs,Cs,Ds,Ps], symbTable=st})
	end

    fun newP (y, {errs, subst, matchTable=[As, Bs, Cs, Ds, Ps], symbTable}) = 
	let val (yp, st) = Var.newBound ((Var.name y)^"P", symbTable)
	    val Ps = VarTab.insert ((y, yp), Ps)
	in
	    (yp, {errs=errs, subst=subst, 
		  matchTable=[As,Bs,Cs,Ds,Ps], symbTable=st})
	end
	    
    fun putError (i, {errs, subst, matchTable, symbTable}) = 
	{errs=VarTab.insert (i, errs), subst=subst,
	 matchTable=matchTable, symbTable=symbTable}
	
    fun putMatch ((v, e, n), {errs, subst, matchTable, symbTable}) =
	{errs=VarTab.insert ((v, e), errs), subst=VarTab.insert ((v, n), subst),
	 matchTable=matchTable, symbTable=symbTable}

    fun getError (v, c:context) =
	case VarTab.find (v, #errs c) of
	    SOME e => e
	  | NONE => raise Error "variable not in context"

    fun getMatch (v, c:context) = 
	case VarTab.find (v, #subst c) of
	    SOME (Target.Var v') => getMatch (v', c)
	  | SOME e => e
	  | NONE => Target.Var v


    fun findMatch (v, t) =
	case VarTab.find (v, t) of
	    SOME v' => v'
	  | NONE => raise Error "variable doesn't have such a match"

    fun getA (v, c:context) = findMatch (v, hd(#matchTable c))
    fun getB (v, c:context) = findMatch (v, List.nth(#matchTable c, 1))
    fun getC (v, c:context) = findMatch (v, List.nth(#matchTable c, 2))
    fun getD (v, c:context) = findMatch (v, List.nth(#matchTable c, 3))
    fun getP (v, c:context) = findMatch (v, List.nth(#matchTable c, 4))


    fun domA (set, c:context) =
	let fun test v =
	    case VarTab.find (v, #errs c) of
		SOME (Oa _) => true
	      | _ => false
	in
	    VarSet.filter test set
	end
    
    fun domB (set, c:context) =
	let fun test v =
	    case VarTab.find (v, #errs c) of
		SOME (Ob _) => true
	      | _ => false
	in
	    VarSet.filter test set
	end

    fun domC (set, c:context) =
	let fun test v =
	    case VarTab.find (v, #errs c) of
		SOME (Oc _) => true
	      | _ => false
	in
	    VarSet.filter test set
	end

    fun domD (set, c:context) =
	let fun test v =
	    case VarTab.find (v, #errs c) of
		SOME Od => true
	      | _ => false
	in
	    VarSet.filter test set
	end

    fun domP (set, c:context) =
	let fun test v =
	    case VarTab.find (v, #errs c) of
		SOME P => true
	      | _ => false
	in
	    VarSet.filter test set
	end


    fun free (v, c:context) = 
	let val Oa q = getError (v, c)
	in 
	    Q.==(q, Q.zero)
	end
	
end


							
	
