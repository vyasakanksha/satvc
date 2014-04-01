module Lexer (
   tokenize
) where

import Token

tokenize :: String -> [Token]

tokenize ('+':rest) = Add:tokenize rest
tokenize ('-':rest) = Sub:tokenize rest
tokenize ('*':rest) = Mul:tokenize rest
tokenize ('/':rest) = Div:tokenize rest
tokenize ('=':rest) = Assign:tokenize rest
tokenize (';':rest) = End:tokenize rest

tokenize ('<':rest) = Less:tokenize rest
tokenize ('>':rest) = Greater:tokenize rest
tokenize ('=':'=':rest) = Equal:tokenize rest
tokenize ('&':'&':rest) = And:tokenize rest
tokenize ('|':'|':rest) = Or:tokenize rest
tokenize ('!':'=':rest) = NEqual:tokenize rest

tokenize ('(':rest) = LParen:tokenize rest
tokenize (')':rest) = RParen:tokenize rest
tokenize ('[':rest) = LBracket:tokenize rest
tokenize (']':rest) = RBracket:tokenize rest
tokenize ('{':rest) = LCurly:tokenize rest
tokenize ('}':rest) = RCurly:tokenize rest
tokenize (',':rest) = Comma:tokenize rest

tokenize ('i':'f':rest) = If:tokenize rest
tokenize ('e':'l':'s':'e':rest) = Else:tokenize rest
tokenize ('e':'l':'i':'f':rest) = Elif:tokenize rest
tokenize ('w':'h':'i':'l':'e':rest) = While:tokenize rest
tokenize ('r':'e':'t':'u':'r':'n':rest) = Return:tokenize rest
tokenize ('f':'u':'n':'c':rest) = Func:tokenize rest

tokenize src@(d:_) | d `elem` digits =
      Num (read num):tokenize rest
   where digits = "0123456789"
         num    = takeWhile (`elem` digits) src
         rest   = dropWhile (`elem` digits) src

tokenize src@(c:_) | c `elem` identChars =
      Funcname name:tokenize rest
   where identChars = '_':'\'':['A'..'Z']
         name   = takeWhile (`elem` identChars) src
         rest   = dropWhile (`elem` identChars) src

tokenize src@(c:_) | c `elem` identChars =
      Varname name:tokenize rest
   where identChars = '_':'\'':['a'..'z']
         name   = takeWhile (`elem` identChars) src
         rest   = dropWhile (`elem` identChars) src

tokenize (' ':rest)  = tokenize rest
tokenize (' ':rest)  = tokenize rest
tokenize (' ':rest)  = tokenize rest
tokenize ('\n':rest) = tokenize rest
tokenize ('\t':rest) = tokenize rest
tokenize [] = [EOF]
