module RenderLib where

import Text.StringTemplate
import System.Process
import System.FilePath
import Data.List

getTitle :: FilePath -> IO (String)
getTitle src = do
    srct2t  <- readFile src
    return $ head $ lines srct2t


renderT2T :: (StringTemplate String -> StringTemplate String) -> FilePath -> FilePath -> IO ()
renderT2T attrs src dst = do
    title <- getTitle src
    srchtml <- readProcess "/usr/bin/txt2tags" [ "-t", "html", "-H", "-i", src, "-o", "-" ] []
    templates <- directoryGroup "layouts" :: IO (STGroup String)
    let Just t = getStringTemplate "default" templates
    let lvl = length (splitPath src) - 1
    let top = if lvl == 0 then "." else (concat $ intersperse "/" $ replicate lvl "..")
    writeFile dst $ toString
        $ attrs
        $ setAttribute "top" top
        $ setAttribute "title" title
        $ setAttribute "content" srchtml
        $ t


