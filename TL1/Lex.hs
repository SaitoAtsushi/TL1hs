module TL1.Lex (Token(..), tokenizeTL1) where

import Data.Int
import Data.Char
import Data.List
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Pos
import Control.Monad
import Numeric

data Token = IdentifierToken String
           | NumericToken Integer
           | SymbolicToken Char
           | StringToken String
           deriving (Show)

x <!> y = try x <|> y

type TL1_Lexer a = Parsec String () a

l_decDigit :: TL1_Lexer Token
l_decDigit = liftM (NumericToken . read) $ many1 digit

l_hexDigit :: TL1_Lexer Token
l_hexDigit = do char '$'
                cs <- many1 hexDigit
                return $ NumericToken $ read ("0x"++cs)

l_charDigit :: TL1_Lexer Token
l_charDigit = do c <- between (char '\'') (char '\'') anyChar
                 return $ NumericToken $ toInteger $ digitToInt c

l_identifier :: TL1_Lexer Token
l_identifier = do c  <- letter
                  cs <- many alphaNum
                  return $ IdentifierToken $ fmap toUpper (c:cs)

l_string :: TL1_Lexer Token
l_string = do char '"'
              s <- manyTill anyChar (try $ char '"')
              return $ StringToken s

l_symbol :: TL1_Lexer Token
l_symbol = do c <- oneOf "=+-*/\\()><[]{}:#,"
              return $ SymbolicToken c

l_token =
  liftM2 (,) getPosition
  (l_symbol <!> l_identifier <!> l_symbol <!> l_charDigit
   <!> l_decDigit <!> l_hexDigit <!> l_string)


l_skip :: TL1_Lexer ()
l_skip = do skipMany (try skipChar)
            option () l_comment
  where
    skipChar = tokenPrim show next test
    test c = if c `elem` " .;" || ord c<0x1f then Just c else Nothing
    next pos x xs = updatePosChar pos x
    l_comment :: TL1_Lexer ()
    l_comment = do char '%'
                   manyTill anyChar $ try endOfLine
                   l_skip
                   return ()

l_tokens :: TL1_Lexer [(SourcePos, Token)]
l_tokens = do l_skip
              tokens <- many (do{t<-l_token; l_skip; return t})
              eof
              return tokens

tokenizeTL1 source name = runParser l_tokens () name source
