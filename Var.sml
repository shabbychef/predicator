(**************************************
 ** Var.sml
 ** sml
 ** 
 ** Aleksandar Nanevski
 **
 ** Implementation of variable tables.
 ** Should ideally be an instance of a 
 ** MULTISET structure, but that makes problems
 ** with the toString function.
 **************************************)

structure Var :> VAR =
struct
    exception Error of string

    type var = string * int
    type t = var

    fun compare ((c1, i1), (c2, i2)) =
	case (String.compare(c1, c2)) of
	    EQUAL => Int.compare(i1, i2)
	  | s => s

    structure Index = 
    struct
	type t = var
	val compare = compare
    end
    
    structure Table = RedBlackTable (structure Index = Index) : TABLE

    datatype bind_mode = FREE | BOUND

    datatype dictEntry = Count of int | Mode of bind_mode

    type dict = dictEntry Table.table

    val empty = Table.empty


    fun stripDigits name =
	case String.tokens Char.isDigit name of
	    (alpha::_) => alpha
	  | _ => raise Error "bad variable name"


    fun newFree (name, symbTable) =
	let val name = stripDigits name
	    fun update (NONE) = (SOME (Count 1), 1)
	      | update (SOME (Count i)) = (SOME (Count (i+1)), i+1)
	    val (symbTable, i) = Table.updateReturn update ((name, 0), symbTable)
	in
	    ((name, i), Table.insert(((name, i), Mode FREE), symbTable))
	end
    
    fun newBound (name, symbTable) =
	let val name = stripDigits name
	    fun update (NONE) = (SOME (Count 1), 1)
	      | update (SOME (Count i)) = (SOME (Count (i+1)), i+1)
	    val (symbTable, i) = Table.updateReturn update ((name, 0), symbTable)
	in
	    ((name, i), Table.insert(((name, i), Mode BOUND), symbTable))
	end

    fun setBind (v, bm, symbTable) =
	let fun update (NONE) = (NONE, 0)
	      | update (SOME _) = (SOME (Mode bm), 1)
	    val (symbTable, i) = Table.updateReturn update (v, symbTable)
	in
	    if i = 0 then raise Error "nonexistent variable"
	    else symbTable
	end
    
    fun getBind (v, symbTable) =
	case Table.find (v, symbTable) of
	    NONE => raise Error "nonexistent variable"
	  | SOME(Mode bm) => bm


    fun name (n, i) = n

    fun isFree (v, symbTable) =
	case Table.find (v, symbTable) of
	    NONE => raise Error "nonexistent variable"
	  | SOME(Mode FREE) => true
	  | SOME(Mode BOUND) => false

    fun toString (c, i) = 
	if i = 1 then String.toString c
	else (String.toString c)^(Int.toString i)

end


