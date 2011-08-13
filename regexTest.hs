{-# LANGUAGE TemplateHaskell #-}
import Regex
import Test.QuickCheck
import Test.QuickCheck.All

alphanumeric :: Gen Char
alphanumeric = elements $  ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

sizes :: Gen Int
sizes = choose (0,10)

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
prop_or2 = forAll alphanumeric $ \c1 -> 
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
             let r1 = (compile $ "(" ++ s ++ ")*") in
             let r2 = (compile $ "(" ++ s ++ ")**") in
             let str = (concat $ take n $ repeat s) in
             (r1 `matches` str) && (r2 `matches` str)

runTests = $quickCheckAll