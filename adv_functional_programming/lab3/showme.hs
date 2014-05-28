instance Show Expr where
	
	show(V t) = show t
	show(VO vop e1 e2) = "{ " ++ show(vop) ++ " , " ++ show(e1) ++ " , " ++ show(e2) ++ " }"
	show(SO sop e1 e2) = "{ " ++ show(sop) ++ " , " ++ show(e1) ++ " , " ++ show(e2) ++ " }"


instance Show IntExpr where
	
	show (I val) =  show(val)
	show (NO nop expr) = "{ " ++ show(nop) ++ " , " ++ show(expr) ++ "}"

instance Show VectorOp where
	
	show Add = "'add'"
	show Sub = "'sub'"
	show Dot = "'dot'"

instance Show ScalarOp where
	
	show Mul = "'mul'"
	show Div = "'div'"

instance Show NormOp where
 	
 	show NormOne = "'norm_one'"  
 	show NormInf = "'norm_inf'"