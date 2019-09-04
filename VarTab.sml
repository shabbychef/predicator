(**********************************
 ** VarTab.sml
 ** sml
 ** 
 ** Aleksandar Nanevski
 **
 ** Tables indexed by variables 
 **********************************)

structure VarTab = RedBlackTable (structure Index = Var) : TABLE
