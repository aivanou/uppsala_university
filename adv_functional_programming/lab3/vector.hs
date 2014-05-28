module Vector where

type Vector   = [Integer]
data Expr     = V Vector
              | VO VectorOp Expr Expr
              | SO ScalarOp IntExpr Expr
data IntExpr  = I Integer
              | NO NormOp Expr
data VectorOp = Add | Sub | Dot
data ScalarOp = Mul | Div
data NormOp   = NormOne
              | NormInf

#include "showme.hs"
