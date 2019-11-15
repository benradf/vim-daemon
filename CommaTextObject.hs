module CommaTextObject where

import Lex
import qualified Data.Map as Map


data Token
  = Comma
  | LeftParen
  | RightParen
  | LeftBox
  | RightBox
  | LeftBrace
  | RightBrace
  | Newline
  | Special String
  deriving Show

lexTree :: LexTree Token
lexTree = makeLexTree $ Map.fromList
  [ (",", Comma)
  , ("(", LeftParen)
  , (")", RightParen)
  , ("[", LeftBox)
  , ("]", RightBox)
  , ("{", LeftBrace)
  , ("}", RightBrace)
  , ("=>", Special "=>")
  , ("<-", Special "<-")
  , ("->", Special "->")
  , ("==>", Special "==>")
  , ("<--", Special "<--")
  , (":=>", Special ":=>")
  ]

{- Write a regression test for the following bug:
    λ runLexer lexTree (makeLocatedString "<-=>")
    [Located 0 (Special "<-")]
-}

lexer :: String -> [Located Token]
lexer = runLexer lexTree . makeLocatedString
