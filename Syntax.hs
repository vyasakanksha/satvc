module Syntax (
   Expr(..),
   BExpr(..),
   Program(..),
   StmtBlock(..),
   Stmt(..),
   IfStmt(..)
) where

data Expr = AddOp Expr Expr
          | SubOp Expr Expr
          | MulOp Expr Expr
          | DivOp Expr Expr
          | Index Expr Expr 
          | Number Integer
          | List [Expr]
          | Variable Varname
          deriving(Eq, Show)

data Value = E Expr
           | L [Value]

type Varname = String

data BExpr = LessOp Expr Expr
           | GreaterOp Expr Expr
           | EqualOp Expr Expr
           | NEqualOp Expr Expr
           | NotOp BExpr BExpr
           | OrOp BExpr BExpr
           | AndOp BExpr BExpr
           | RExpr
           deriving(Eq, Show)

data IfStmt = IfOp Expr [Stmt]
            | ElseOp [Stmt]
            | ElifOp Expr [Stmt]
            deriving Show

data Stmt = Expr
          | AssignmentOp Expr Expr
          | WhileOp BExpr [Stmt]
          | IfStmt
   deriving Show

--data Program = Program [Assignment] Expr deriving Show
data StmtBlock = FuncOp [Varname] [Stmt] Expr deriving Show

data Program = Program [StmtBlock] deriving Show

