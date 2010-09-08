
import System
import Data.ByteString.Lazy (ByteString)
import Text.StringTemplate
import System.Process


main :: IO ()
main = do
    [src,dst] <- getArgs
    srchtml <- readProcess "/usr/bin/txt2tags" [ "-t", "html", "-H", "-i", src, "-o", "-" ] []
    templates <- directoryGroup "layouts" :: IO (STGroup String)
    let Just t = getStringTemplate "default" templates
    writeFile dst $ toString
        $ setAttribute "top" "."
        $ setAttribute "content" srchtml
        $ t

