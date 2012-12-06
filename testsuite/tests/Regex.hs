import Regex
import qualified Regex.Internal as Internal
import qualified Regex.Parser as Parser
import Regex.Parser (AST (Empty, Lit, Or, Concat, Star), RawRegex, Alphabet)

import System.Random

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Gen

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
          testGroup "Internal API" [
            testProperty "general" prop_general
          ],
          testGroup "Public API" [
            testProperty "lit1" prop_lit1,
            testProperty "lit2" prop_lit2,
            testProperty "lit3" prop_lit3,
            testProperty "or1" prop_or1,
            testProperty "opt1" prop_opt1,
            testProperty "starOr" prop_starOr,
            testProperty "concat1" prop_concat1,
            testProperty "star1" prop_star1,
            testProperty "plus1" prop_plus1,
            testProperty "plus2" prop_plus2
        ]
      ]

alphanumeric :: Gen Char
alphanumeric = elements $  ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

sizes :: Gen Int
sizes = choose (0,10)

-- | Pair of an AST and a String that should match the AST's correspoding regular expression.
data MatchPair = MatchPair AST String deriving Show

-- | Arbitrary instance for 'MatchPair'
instance Arbitrary MatchPair where
    -- Shrinking after-the-fact is hard, so we'll just cap the size here at time of generation.
    arbitrary = MkGen $ \g n -> fst $ go g (n `mod` 7)
      where go :: StdGen -> Int -> (MatchPair, StdGen)
            go g 0 = (MatchPair Empty "", g)
            go g 1 = let (g', g'') = split g in
                     let c = (unGen alphanumeric) g' 1 in
                     (MatchPair (Lit c) (c:[]), g'')
            go g n = -- Random value for choosing constructor:
                     let (val, g1) = next g in
                     -- Lazy sub-ASTs used by each constructor:
                     let (MatchPair ast1 s1, g2) = go g1 (n-1) in
                     let (MatchPair ast2 s2, g3) = go g2 (n-1) in
                     -- Two generators: one to return, one to use for choosing matched input.
                     let (g4, g5) = split g3 in
                     let matchPair = case val `mod` 3 of 
                                      0 -> let s = unGen (elements ["", s1, s1 ++ s1]) g4 1 in
                                           MatchPair (Star ast1) s
                                      1 -> MatchPair (Concat ast1 ast2) (s1 ++ s2)
                                      2 -> let s = unGen (elements [s1, s2]) g4 1 in
                                           MatchPair (Or ast1 ast2) s
                      in (matchPair, g5)
    shrink = shrinkNothing

-- | Tests general matches using the above 'Arbitrary' Instance
prop_general :: MatchPair -> Bool
prop_general (MatchPair ast s) = (Internal.assemble ast) `matches` s

prop_lit1 c = (compile ['\\', c]) `matches` [c]
prop_lit2 = forAll (listOf alphanumeric) $ \s -> 
            (compile s) `matches` s
prop_lit3 = forAll alphanumeric $ \c1 ->
            forAll alphanumeric $ \c2 ->
            c1 /= c2 ==> not $ (compile [c1]) `matches` [c2]

prop_or1 = forAll alphanumeric $ \c1 ->
           forAll alphanumeric $ \c2 ->
           let regex = compile [c1, '|', c2] in
            (regex `matches` [c1]) && (regex `matches` [c2])

prop_opt1 = forAll alphanumeric $ \c1 ->
            let regex = compile [c1, '?'] in
            (regex `matches` []) && (regex `matches` [c1]) && not (regex `matches` [c1, c1])

-- | Tests that Star distributes across Or.
prop_starOr = forAll alphanumeric $ \c1 -> 
              forAll alphanumeric $ \c2 ->
              forAll (listOf (elements [c1, c2])) $ \s ->
              let regex = compile ['(', c1, '|', c2, ')', '*'] in
              regex `matches` s

prop_concat1 = forAll (listOf alphanumeric) $ \s1 ->
               forAll (listOf alphanumeric) $ \s2 ->
               (compile $ s1 ++ s2) `matches` (s1 ++ s2)

prop_star1 = forAll sizes $ \n -> 
             forAll (listOf alphanumeric) $ \s ->
             not (null s) ==>
             let r = (compile $ "(" ++ s ++ ")*") in
             let str = (concat $ replicate n s) in
             (r `matches` str)
             
prop_plus1 = forAll sizes $ \n -> 
             forAll (listOf alphanumeric) $ \s ->
             not (null s) && (n /= 0) ==>
             let r = (compile $ "(" ++ s ++ ")+") in
             let str = (concat $ replicate n s) in
             (r `matches` str)
             
prop_plus2 = forAll alphanumeric $ \c ->
             let r = compile [c, '+'] in
             not $ r `matches` ""
