
import System
import Data.ByteString.Lazy (ByteString)
import Text.StringTemplate

main :: IO ()
main = do
    [src,dst] <- getArgs
    templates <- directoryGroup "_srcs/layouts" :: IO (STGroup String)
    let Just t = getStringTemplate "default" templates
    f <- readFile src
    writeFile dst $ toString $ setAttribute "content" f $ t

