
import System
import Data.ByteString.Lazy (ByteString)
import Text.StringTemplate
import System.Process
import System.FilePath
import Data.List

main :: IO ()
main = do
    [layoutdir, lvlstr, src, dst] <- getArgs
    let lvl = read lvlstr :: Int
    srct2t  <- readFile src
    let title = head $ lines srct2t
    srchtml <- readProcess "/usr/bin/txt2tags" [ "-t", "html", "-H", "-i", src, "-o", "-" ] []
    templates <- directoryGroup (joinPath [ layoutdir, "layouts" ]) :: IO (STGroup String)
    let Just t = getStringTemplate "default" templates
    let top = if lvl == 0 then "." else (concat $ intersperse "/" $ replicate lvl "..")
    writeFile dst $ toString
        $ setAttribute "top" top
        $ setAttribute "title" title
        $ setAttribute "content" srchtml
        $ t

