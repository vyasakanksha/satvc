module Token (
   Token(..),
	sametype
) where

data Token = Add
           | Sub
           | Mul
           | Div
           | Assign 
           | End
           | Return 
           | Func
           | Less
           | Greater
           | Equal
           | And
           | Or 
           | NEqual
           | LParen
           | RParen
           | LBracket
           | RBracket
           | LCurly
           | RCurly
           | Comma
           | Funcname String
           | Varname String
           | If 
           | Else 
           | Elif
           | While
           | Num Integer
			  | EOF
           deriving(Show)

sametype :: Token -> Token -> Bool

sametype Add         Add          = True
sametype Mul         Mul          = True
sametype Sub         Sub          = True
sametype Div         Div          = True
sametype End         End          = True
sametype Assign      Assign       = True
sametype Return      Return       = True
sametype Func        Func         = True
sametype Less        Less         = True
sametype Greater     Greater      = True
sametype Equal       Equal        = True
sametype NEqual      NEqual       = True
sametype And         And          = True
sametype Or          Or           = True
sametype LParen      LParen       = True
sametype RParen      RParen       = True
sametype LBracket    RBracket     = True
sametype RBracket    RBracket     = True
sametype LCurly      LCurly       = True
sametype RCurly      RCurly       = True
sametype Comma       Comma        = True
sametype If          If           = True
sametype Else        Else         = True
sametype Elif        Elif         = True
sametype While       While        = True
sametype (Num _)     (Num _)      = True
sametype (Varname _) (Varname _)  = True
sametype (Funcname _)(Funcname _) = True
sametype _           _            = False
