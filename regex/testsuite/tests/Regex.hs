import Regex
import qualified Regex.Internal as Internal
import qualified Regex.Parser as Parser
import Regex.Parser (AST (Empty, Lit, Or, Concat, Star), RawRegex, Alphabet)

import Control.Monad.State
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
                testProperty "general" prop_general,
                testProperty "general_or" prop_general_or,
                testProperty "general_start" prop_general_star      
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

-- | Pair of an AST and a String that should match the AST's corresponding regular expression.
data MatchPair = MatchPair AST String deriving Show

type RandomState = State StdGen
-- | Generate a new random int, updating the state of the random generator accordingly.
rand :: RandomState Int
rand = do (n, g') <- fmap next get
          put g'
          return n

-- | Splits the random generator kept in the state, returning one and storing another.
newGen :: RandomState StdGen
newGen = do (g', g'') <- fmap split get
            put g''
            return g'

-- | Arbitrary instance for 'MatchPair'
instance Arbitrary MatchPair where
    -- Shrinking after-the-fact is hard, so we'll just cap the size here at time of generation.
    arbitrary = MkGen $ \g n -> evalState (go $ n `mod` 7) g
      where go :: Int -> RandomState MatchPair
            go 0 = return $ MatchPair Empty ""
            go 1 = do g <- newGen
                      let c = (unGen alphanumeric) g 1
                      return $ MatchPair (Lit c) (c:[])
            go n = do (MatchPair ast1 s1) <- go (n-1)
                      (MatchPair ast2 s2) <- go (n-1)
                      val <- rand
                      case val `mod` 3 of
                        0 -> do g <- newGen
                                let s = unGen (elements ["", s1, s1 ++ s1]) g 1
                                return $ MatchPair (Star ast1) s
                        1 -> return $ MatchPair (Concat ast1 ast2) (s1 ++ s2)
                        2 -> do g <- newGen
                                let s = unGen (elements [s1, s2]) g 1
                                return $ MatchPair (Or ast1 ast2) s 
    shrink = shrinkNothing

-- | Tests general matches using the above 'Arbitrary' Instance
prop_general :: MatchPair -> Bool
prop_general (MatchPair ast s) = (Internal.assemble ast) `matches` s

prop_general_or :: MatchPair -> MatchPair -> Bool
prop_general_or (MatchPair ast1 s1) (MatchPair ast2 s2) = let r = Internal.assemble (Or ast1 ast2)
                                                          in r `matches` s1 && r `matches` s2

prop_general_star :: MatchPair -> Bool
prop_general_star (MatchPair ast s) = let r = Internal.assemble (Star ast)
                                      in r `matches` (s ++ s) && r `matches` ""

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
