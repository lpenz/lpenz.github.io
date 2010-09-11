module RenderLib where

import Text.StringTemplate
import System.Process
import System.FilePath
import Data.List
import Control.Monad


getTitle :: FilePath -> IO (String)
getTitle src = do
    srct2t  <- readFile src
    return $ head $ lines srct2t


renderT2T :: (StringTemplate String -> StringTemplate String) -> FilePath -> FilePath -> FilePath -> IO ()
renderT2T attrs tpfile src dst = do
    title <- getTitle src
    srchtml <- readProcess "/usr/bin/txt2tags" [ "-t", "html", "-H", "-i", src, "-o", "-" ] []
    tp <- liftM newSTMP $ readFile tpfile
    let lvl = length (splitPath src) - 1
    let top = if lvl == 0 then "." else (concat $ intersperse "/" $ replicate lvl "..")
    writeFile dst $ toString
        $ attrs
        $ setAttribute "top" top
        $ setAttribute "title" title
        $ setAttribute "content" srchtml
        $ tp


