
-- | Internal parser for regular expressions.
module Regex.Parser where

-- | An unparsed regular expression.
type RawRegex = String

-- | Type representing the set of characters that can be matched by a Regex.
type Alphabet = Char

-- TODO: Extend to allow marking of matching groups, with indexes.
-- | Abstract syntax tree for parsed regular expressions.
data AST = Empty -- ^ Matches the empty string.
         | Lit !Alphabet -- ^ Matches a single character.
         | Star !AST -- ^ Matches zero or more occurrences of the subtree.
         | Concat !AST !AST -- ^ Matches the first tree followed by the second tree.
         | Or !AST !AST -- ^ Matches either the first or second tree.
         deriving Show

type TokenStack = [AST]

-- TODO: Error reporting
-- TODO: Replace this with something prettier
-- | Parses a regular expression using a stack of tokens.
stackParse :: TokenStack -- ^ Stack of tokens processed so far. 
           -> RawRegex -- ^ Remainder of input string.
           -> (AST, RawRegex) -- ^ Resulting AST and the unparsed remainder of the input string.
stackParse _      ('\\':[])      = undefined
stackParse ts     ('\\':a:as)    = stackParse ((Lit a):ts) as
--TODO: Mark the AST parsed as being part of a matching group, indexed appropriately.
stackParse ts     ('(':as)       = case stackParse [] as of
                                    (x, as') -> stackParse (x:ts) as'
stackParse []     (')':_)        = undefined                                   
stackParse ts     (')':as)       = (concatStack ts, as)
stackParse []     ('*':_)        = undefined
stackParse (x:ts) ('*':as)       = stackParse ((Star x):ts) as
stackParse []     ('+':_)        = undefined
stackParse (x:ts) ('+':as)       = stackParse ((Concat x (Star x)):ts) as
stackParse []     ('?':_)        = undefined
stackParse (x:ts) ('?':as)       = stackParse ((Or x Empty):ts) as
stackParse []     ('|':_)        = undefined
stackParse ts     ('|':as)       = case (stackParse ts [], stackParse [] as) of
                                    ((left, []), (right, remainder)) -> stackParse ((Or left right):[]) remainder
stackParse ts     (a:as)         = stackParse ((Lit a):ts) as
stackParse []     []             = (Empty, [])
stackParse ts     []             = (concatStack ts, [])
  
-- | Concatenates the contents of a stack.
concatStack :: [AST] -> AST
concatStack ts = foldr1 Concat (reverse ts)
