
import Regex
import System (getArgs)

pathological n = matches (compile $ (recurse n) ++ (take n $ repeat 'a')) (take n $ repeat 'a')
    where recurse 0 = ""
          recurse n = "a?" ++ (recurse (n-1))
          
main = do (x:xs) <- getArgs
          let n = read x
          print $ pathological n