
import Regex
import Data.GraphViz.Commands
import Data.ByteString as B
import IO

r = compile "a*"

main = do graphvizWithHandle Dot (toDot r) Svg write
            where write :: Handle -> IO ()
                  write h = (B.hGetContents h) >>= (B.hPut stdout)