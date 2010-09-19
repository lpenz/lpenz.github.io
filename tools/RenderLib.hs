module RenderLib where

import Text.StringTemplate
import System.Directory
import System.Process
import System.FilePath
import Data.List
import Control.Monad
import Data.Maybe
import Text.Printf (printf)


getFileTitle :: FilePath -> IO (String)
getFileTitle src = do
    srct2t <- readFile src
    return $ head $ lines srct2t


getBreadcrumbs :: String -> FilePath -> IO (String)
getBreadcrumbs top src = do
    let paths = inits $ init $ splitPath src
    title <- mapM gettitles paths
    let titleurl = map geturl (zip paths title)
    --putStrLn $ show paths ++ " got " ++ show titleurl
    let rv = concat $ intersperse " &gt; " titleurl
    --putStrLn $ "rv "++rv
    return $ rv
    where
        geturl :: ([String], String) -> String
        geturl (p0, t0) = printf "<a href=\"%s/%sindex.html\">%s</a>" top (concat p0) t0
        gettitles p0 = do
            let p = concat p0
            let f = p ++ "./index.t2t"
            e <- doesFileExist f
            t <- getFileTitle f
            let r = if e then t else (last p0)
            return r

		-- <a href="$top$/index.html">Avulsos by Penz</a>

renderT2T :: (StringTemplate String -> StringTemplate String) -> FilePath -> FilePath -> FilePath -> IO ()
renderT2T attrs tpfile src dst = do
    title <- getFileTitle src
    srchtml <- readProcess "/usr/bin/txt2tags" [ "-t", "html", "-H", "-i", src, "-o", "-" ] []
    tp0 <- liftM newSTMP $ readFile tpfile
    let lvl = length (splitPath src) - 1
    let top = if lvl == 0 then "." else (concat $ intersperse "/" $ replicate lvl "..")
    breadcrumbs <- getBreadcrumbs top src
    let tp =
            attrs
            $ setAttribute "top" top
            $ setAttribute "title" title
            $ setAttribute "content" srchtml
            $ setAttribute "breadcrumbs" breadcrumbs
            $ tp0
    let (err, _, _) = checkTemplate tp
    when (isJust err) $ error $ fromJust err
    writeFile dst $ toString tp


