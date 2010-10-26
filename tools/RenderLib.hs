module RenderLib where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import System.Directory
import System.FilePath
import System.Locale
import System.Process
import Text.Printf (printf)
import Text.StringTemplate
import Data.List.Utils


home :: String
home = "http://lpenz.github.com"


formatdayrfc :: Day -> String
formatdayrfc d = f $ formatTime defaultTimeLocale rfc822DateFormat $ UTCTime { utctDay = d, utctDayTime = fromInteger 0 }
    where
        f (' ':'U':'T':'C':[]) = " GMT"
        f (x:xs) = x:f xs
        f [] = []


t2tToHtml :: String -> IO (String)
t2tToHtml str = do
    let nstr = "\n\n\n\n" ++ str
    readProcess "/usr/bin/txt2tags" [ "-t", "html", "-H", "-i", "-", "-o", "-" ] nstr

t2tfileToHtml :: FilePath -> IO (String)
t2tfileToHtml filename = readProcess "/usr/bin/txt2tags" [ "-t", "html", "-H", "-i", filename, "-o", "-" ] []


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
    srchtml0 <- t2tfileToHtml src
    let srchtml = replace "$cwd$/" "" srchtml0
    tp0 <- liftM newSTMP $ readFile tpfile
    let lvl = length (splitPath src) - 1
    let top = if lvl == 0 then "." else (concat $ intersperse "/" $ replicate lvl "..")
    breadcrumbs <- getBreadcrumbs top src
    let tp =
            attrs
            $ setAttribute "top" top
            $ setAttribute "home" "http://lpenz.github.com"
            $ setAttribute "title" title
            $ setAttribute "content" srchtml
            $ setAttribute "breadcrumbs" breadcrumbs
            $ tp0
    let (err, _, _) = checkTemplate tp
    when (isJust err) $ error $ fromJust err
    writeFile dst $ toString tp


