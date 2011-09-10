
import Regex
import System (getArgs)

-- A known case of poor performance for many regular expression implementations.
pathological n = matches (compile $ (concat $ nOf "a?") ++ (nOf 'a')) (nOf 'a')
    where nOf = replicate n
    
main = do (x:xs) <- getArgs
          let n = read x
          print $ pathological n