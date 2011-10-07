type id = string

type operator = Plus | Minus | Times | Div | Mod
type intExpr = Var of id | Int of int | Operation of Operator * IntExpr * IntExpr

type range = { min : intExpr ; max : intExpr }

type fil_lvalue = 



