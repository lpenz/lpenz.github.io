
import System
import Data.ByteString.Lazy (ByteString)
import Text.StringTemplate
import System.Process
import System.FilePath
import Data.List

getTitle :: FilePath -> IO (String)
getTitle src = do
    srct2t  <- readFile src
    return $ head $ lines srct2t


main :: IO ()
main = do
    [src, dst] <- getArgs
    title <- getTitle src
    srchtml <- readProcess "/usr/bin/txt2tags" [ "-t", "html", "-H", "-i", src, "-o", "-" ] []
    templates <- directoryGroup "layouts" :: IO (STGroup String)
    let Just t = getStringTemplate "default" templates
    let lvl = length (splitPath src) - 1
    let top = if lvl == 0 then "." else (concat $ intersperse "/" $ replicate lvl "..")
    writeFile dst $ toString
        $ setAttribute "top" top
        $ setAttribute "title" title
        $ setAttribute "content" srchtml
        $ t

