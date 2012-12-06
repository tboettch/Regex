
import Regex.Internal
import Regex.Parser
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit.Base((@=?))

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
          testCase "parseEmpty" test_empty,
          testCase "parseLit" test_lit,
          testCase "parseConcat" test_concat,
          testCase "star" test_star,
          testCase "plus" test_plus,
          testCase "or" test_or,
          testCase "optional" test_optional,
          testCase "precedence1" test_precedence1,
          testCase "precedence2" test_precedence1,
          testCase "precedence3" test_precedence1,
          testCase "parens" test_parens
      ]

test_empty = Empty @=? parse ""
test_lit = Lit 'a' @=? parse "a"
test_concat = Concat (Lit 'a') (Lit 'b') @=? parse "ab"
test_star = Star (Lit 'a') @=? parse "a*"
test_plus = Concat (Lit 'a') (Star (Lit 'a')) @=? parse "a+"
test_or = Or (Lit 'a') (Lit 'b') @=? parse "a|b"
test_optional = Or (Lit 'a') Empty @=? parse "a?"
test_precedence1 = Or (Lit 'a') (Concat (Lit 'b') (Star (Lit 'c'))) @=? parse "a|bc*"
test_precedence2 = Or (Lit 'a') (Concat (Lit 'b') (Concat (Lit 'c') (Star (Lit 'c')))) @=?parse "a|bc+"
test_precedence3 = Or (Lit 'a') (Concat (Lit 'b') (Or (Lit 'c') Empty)) @=? parse "a|bc?"
test_parens = Star (Or (Lit 'a') (Lit 'b')) @=? parse "(a|b)*"
