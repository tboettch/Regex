
-- | Internal parser for regular expressions.
module Regex.Parser where

import Prelude hiding (concat)
import Data.List (foldl1')
import Control.Applicative hiding ((<|>))
import Text.Parsec hiding (token, tokens, Empty)
import qualified Text.Parsec as Parsec
import Text.Parsec.String(Parser)

-- | An unparsed regular expression.
type RawRegex = [Alphabet]

-- | Type representing the set of characters that can be matched by a Regex.
type Alphabet = Char

-- TODO: Extend to allow marking of matching groups, with indexes.
-- | Abstract syntax tree for parsed regular expressions.
data AST = Empty -- ^ Matches the empty string.
         | Lit !Alphabet -- ^ Matches a single character.
         | Star !AST -- ^ Matches zero or more occurrences of the subtree.
         | Concat !AST !AST -- ^ Matches the first tree followed by the second tree.
         | Or !AST !AST -- ^ Matches either the first or second tree.
         deriving (Show, Eq)

-- | Converts a raw expression into an 'AST', or an error if the raw expression is syntactically invalid.
parse :: RawRegex -- ^ The raw expression
      -> Either String AST -- ^ The parsed regex ('Right') or an error message ('Left').
parse input = case Parsec.parse fullRegex "" input of
                (Left err)  -> Left $ show err
                (Right val) -> Right val

reservedTokens :: [Alphabet]
reservedTokens = ['+', '?', '*', '(', ')', '|', '\\']

-- | Parses the provided parser surrounded by parens.
parens :: Parser a -> Parser a
parens p = char '(' *> p <* char ')'


-- | Parser for the the full regular expression, allowing for the empty string but otherwise requiring the full input to be consumed.
fullRegex :: Parser AST
fullRegex =   eof *> pure Empty
          <|> regex <* eof

regex :: Parser AST
regex =  fmap (foldl1' Or) $ terms `sepBy1` char '|'

terms :: Parser AST
terms = fmap (foldl1' Concat) $ many1 term

term :: Parser AST
term =   try star
     <|> try plus
     <|> try opt
     <|> token
      
star :: Parser AST
star = fmap Star (token <* char '*')

plus :: Parser AST
plus = do t <- token
          char '+'
          return $ Concat t (Star t)

opt :: Parser AST
opt = fmap (Or Empty) (token <* char '?')

token :: Parser AST
token =   lit
      <|> parens regex
      <?> "a literal character or a parenthesized expression"

lit :: Parser AST
lit = (fmap Lit $     noneOf reservedTokens
                 <|> (char '\\' *> anyChar))
     <?> "a normal character or a slash preceding a reserved character"
