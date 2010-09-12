module RenderLib where

import Text.StringTemplate
import System.Process
import System.FilePath
import Data.List
import Control.Monad
import Data.Maybe


getFileTitle :: FilePath -> IO (String)
getFileTitle src = do
    srct2t  <- readFile src
    return $ head $ lines srct2t




renderT2T :: (StringTemplate String -> StringTemplate String) -> FilePath -> FilePath -> FilePath -> IO ()
renderT2T attrs tpfile src dst = do
    title <- getFileTitle src
    srchtml <- readProcess "/usr/bin/txt2tags" [ "-t", "html", "-H", "-i", src, "-o", "-" ] []
    tp0 <- liftM newSTMP $ readFile tpfile
    let lvl = length (splitPath src) - 1
    let top = if lvl == 0 then "." else (concat $ intersperse "/" $ replicate lvl "..")
    let tp =
            attrs
            $ setAttribute "top" top
            $ setAttribute "title" title
            $ setAttribute "content" srchtml
            $ tp0
    let (err, _, _) = checkTemplate tp
    when (isJust err) $ error $ fromJust err
    writeFile dst $ toString tp


