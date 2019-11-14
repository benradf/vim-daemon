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
  ]

lexer :: String -> [Token]
lexer = runLexer lexTree
