module Evaluate (
	evaluate
) where

import Syntax

evaluate :: Expr -> Integer

evaluate (AddOp a b) = evaluate a + evaluate b
evaluate (SubOp a b) = evaluate a - evaluate b
evaluate (MulOp a b) = evaluate a * evaluate b
evaluate (DivOp a b) = evaluate a `div` evaluate b
evaluate (Number x)  = x

evaluate (Assign var value) = 
