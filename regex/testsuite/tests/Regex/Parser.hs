
import Regex.Internal
import Regex.Parser
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit.Base((@=?))

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testCase "empty" test_empty
        , testCase "lit" test_lit
        , testCase "concat1" test_concat1
        , testCase "concat2" test_concat2
        , testCase "star" test_star
        , testCase "plus" test_plus
        , testCase "or" test_or
        , testCase "optional" test_optional
        , testCase "precedence1" test_precedence1
        , testCase "precedence2" test_precedence2
        , testCase "precedence3" test_precedence3
        , testCase "precedence4" test_precedence4
        , testCase "parens1" test_parens1
        , testCase "parens2" test_parens2
        , testCase "parens3" test_parens3
        ]

test_empty = Empty @=? parse ""
test_lit = Lit 'a' @=? parse "a"
test_concat1 = Concat (Lit 'a') (Lit 'b') @=? parse "ab"
test_concat2 = Concat (Star $ Lit 'a') (Lit 'b') @=? parse "a*b"
test_star = Star (Lit 'a') @=? parse "a*"
test_plus = Concat (Lit 'a') (Star (Lit 'a')) @=? parse "a+"
test_or = Or (Lit 'a') (Lit 'b') @=? parse "a|b"
test_optional = Or Empty (Lit 'a') @=? parse "a?"
test_precedence1 = Or (Lit 'a') (Concat (Lit 'b') (Star (Lit 'c'))) @=? parse "a|bc*"
test_precedence2 = Or (Lit 'a') (Concat (Lit 'b') (Concat (Lit 'c') (Star (Lit 'c')))) @=?parse "a|bc+"
test_precedence3 = Or (Lit 'a') (Concat (Lit 'b') (Or Empty (Lit 'c'))) @=? parse "a|bc?"
test_associativity1 = Or (Or (Concat (Lit 'a') (Lit 'b')) (Concat (Lit 'c') (Lit 'd'))) (Concat (Lit 'e') (Lit 'f')) @=? parse "ab|cd|ef"
test_precedence4 = Or (Concat (Lit 'a') (Lit 'b')) (Lit 'c') @=? parse "ab|c"
test_parens1 = Star (Or (Lit 'a') (Lit 'b')) @=? parse "(a|b)*"
test_parens2 = Lit 'a' @=? parse "(a)"
test_parens3 = Lit 'a' @=? parse "((a))"
