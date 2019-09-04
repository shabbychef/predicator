(**********************************
 ** VarSet.sml
 ** sml
 ** 
 ** Aleksandar Nanevski
 **
 ** Sets of variables 
 **********************************)

structure VarSet :> SET where type index = Var.var = 
struct
    structure VS = SetFromTable (structure Table = VarTab) : SET
    open VS

    fun union (vs1, vs2) = List.foldl insert vs1 (toList vs2)
    fun filter f vs =
	let fun upd (v, vs) =
	    if (f v) then insert (v, vs) else vs
	in
	    foldl upd empty vs
	end
end