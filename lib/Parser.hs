module Parser where 


import Types

import Data.Bits 

import Data.Void (Void)
import Control.Monad (void)
import Control.Monad.Combinators
import Text.Megaparsec 
import Text.Megaparsec.Char 
import Text.Megaparsec.Char.Lexer as L 
import Control.Monad.Combinators.Expr 


type Parser = Parsec Void String

spaceParser :: Parser ()
spaceParser = L.space hspace1 empty empty 

lexemeParser :: Parser a -> Parser a 
lexemeParser = lexeme spaceParser

oneParser :: Parser Char 
oneParser = char '1' 
zeroParser :: Parser Char 
zeroParser = char '0'

{-
expr = makeExprParser term table <?> "expression"
term = parens expr <|> parseBin <?> "term"

table = [[prefix]]
-}

parseBin :: Parser (Bin Bool)
parseBin = do 
    spaceParser
    input <- (many ((\c -> if c == '1' then True else False) <$> (choice [oneParser, zeroParser]))) <* eof
    lexemeParser (pure $ Bin $ input)
