-- Make it more efficient 
-- Get Grammers checked 
-- Expr type for the rest
-- ask about the evaluator - both for formal as well as for the rest

module Parser (
	parser
) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos

import Syntax
import Token

-- Begin silly boilerplate

type ExprParser a = GenParser Token () a

mytoken :: (Token -> Maybe a) -> ExprParser a

mytoken test = token showToken posToken testToken
   where showToken tok = show tok
         posToken  tok = newPos "<stdin>" 0 0
         testToken tok = test tok


match :: Token -> ExprParser Token

match t = mytoken checkEq
   where checkEq tok = if sametype t tok
                           then Just tok
                           else Nothing

-- End silly boilerplate

parens :: ExprParser a -> ExprParser a

parens p =
   do match LParen
      x <- p
      match RParen
      return x


parser :: [Token] -> Either ParseError Program

parser toks = runParser program () "filename" toks


-- A  = M A'
-- A' = + A | - A | epsilon
-- M  = F M'
-- M' = * M | / M | epsilon
-- F  = num | ( A )

topLvlExpr :: ExprParser Expr
topLvlExpr = addExpr <|> listExpr

genExpr :: Expr  -- LHS Expr
           -> Token 
           -> ExprParser Expr -- RHS Parser
           -> (Expr -> Expr -> Expr)
           -> ExprParser Expr

genExpr lhs tok rhs_p cons = 
   do match tok
      rhs <- rhs_p
      return (cons lhs rhs)

genBExpr :: BExpr  -- LHS Expr
           -> Token 
           -> ExprParser BExpr -- RHS Parser
           -> (BExpr -> BExpr -> BExpr)
           -> ExprParser BExpr

genBExpr lhs tok rhs_p cons = 
   do match tok
      rhs <- rhs_p
      return (cons lhs rhs)

genRExpr :: Token 
           -> ExprParser BExpr -- RHS Parser
           -> (Expr -> Expr -> BExpr)
           -> ExprParser BExpr

genBExpr lhs tok rhs_p cons = 
   do match tok
      rhs <- rhs_p
      return (cons lhs rhs)



addExpr :: ExprParser Expr
addExpr =
   do lhs <- mulExpr
      addExpr' lhs


addExpr' :: Expr -> ExprParser Expr

addExpr' lhs =  genExpr lhs Add addExpr AddOp
            <|> genExpr lhs Sub addExpr SubOp
            <|> return lhs

mulExpr :: ExprParser Expr

mulExpr =
   do lhs <- factorExpr
      mulExpr' lhs

mulExpr' :: Expr -> ExprParser Expr

mulExpr' lhs =  genExpr lhs Mul addExpr MulOp
            <|> genExpr lhs Div addExpr DivOp
            <|> return lhs

factorExpr :: ExprParser Expr

factorExpr =  numFactor 
          <|> parens addExpr
          <|> indexExpr
   where numFactor = do (Num x) <- match (Num 0)
                        return (Number x)
-- B  = A O
-- O  = or rE | epsilon
-- A  = A' relE
-- A' = or rE | epsilon
-- relE = < E | > E | == E | <= E | >= E | != E

bExpr :: ExprParser BExpr
bExpr =  do lhs <- andExpr
            orExpr lhs

orExpr :: BExpr -> ExprParser BExpr
orExpr lhs =  genBExpr lhs Or bExpr OrOp
          <|> return lhs

andExpr :: ExprParser BExpr
andExpr =  do lhs <- rExpr
              andExpr' lhs

andExpr' :: BExpr -> ExprParser BExpr
andExpr' lhs =  genBExpr lhs And bExpr AndOp
            <|> return lhs

rExpr :: BExpr -> ExprParser BExpr 
rExpr lhs =  rExpr' 
         <|> parens bExpr
   

do genRExpr Less addExpr GreaterOp
     <|> genRExpr Greater addExpr GreaterOp
     <|> genRExpr Equal addExpr EqualOp
     <|> genRExpr lhs NEqual addExpr NEqualOp
     <|> return lhs

--indexExpr = ident indexExpr'
--indexExpr' = '[' expr ']' | ident | epsilon

indexExpr = 
   do (Varname str) <- match (Varname "")
      indexExpr' (Variable str) 
   where indexExpr' lhs = hasIndex lhs <|> return lhs
         hasIndex lhs = do match LBracket 
                           index <- addExpr
                           match RBracket
                           indexExpr' (Index lhs index)


-- O  = A O'
-- O' = Or O | epsilon
-- A  = E A'
-- A' = And A | epsilon
-- E  = L E'
-- E' = Eq E | NEq E | epsilon
-- L  = T L' | F 
-- L' = > L | < L | >= L | <= L | epsilon
-- T  = Expr | ( O )
-- F = True | False

listExpr :: ExprParser Expr
listExpr = do match LBracket 
              ls <- sepBy topLvlExpr (match (Comma))
              match RBracket
              return (List ls)


onlyIfStmt :: ExprParser IfStmt
onlyIfStmt = do match If
                cond <- addExpr
                match LCurly
                ifbl <- many stmt
                match RCurly
                return (IfOp cond ifbl)

elseStmt :: ExprParser IfStmt
elseStmt = do match Else
              match LCurly
              elseBl <- many stmt
              match RCurly
              return (ElseOp elseBl)

elifStmt :: ExprParser IfStmt
elifStmt = do match Elif
              cond <- addExpr
              match LCurly
              elifBl <- many stmt
              match RCurly
              return (ElifOp cond elifBl)
           
stmt :: ExprParser Stmt
stmt = whileStmt <|> assignment
   where whileStmt = do match While
                        cond <- bExpr
                        match LCurly
                        bl <- many stmt
                        match RCurly
                        return (WhileOp cond bl)
         assignment = do lhs <- indexExpr
                         match Assign
                         rhs <- topLvlExpr
                         return (AssignmentOp lhs rhs)

stmtBlock :: ExprParser StmtBlock
stmtBlock = do match Func
               Funcname fname <- match (Funcname "")
               match LParen 
               ([Varname args]) <- sepBy (match (Varname "")) (match (Comma))
               match RParen
               match LCurly
               pg <- many stmt
               match Return
               retval <- addExpr
               match RCurly
               return (FuncOp [args] pg retval)

program :: ExprParser Program 
program = do retval <- many stmtBlock
             return (Program retval)

--import qualified Data.Map as M

