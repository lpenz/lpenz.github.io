module Main where

import System
import System.IO
import Data.Time.Calendar


news :: [(Day, String)]
news = [
    ( fromGregorian 2010 10 23,
        "Main page now has a //whatsnew// section!")
    ,(fromGregorian 2010  8 15,
        "New [Data exploring with R: hard drive occupation prediction articles/df0pred-1/index.html] article.")
    ,(fromGregorian 2010  4 11,
        "New [Debianization with git-buildpackage articles/debgit/index.html] article.")
    ]



indexbuild :: Handle -> IO ()
indexbuild h = do
    hPutStr h "Avulsos by Penz\n"
    hPutStr h $ replicate 3 '\n'
    hPutStr h "= Contents =\n\n\n"
    hPutStr h $ concat [
        "This page acts as a central hub to stuff I write. There is no central theme, anything goes. For now, you can find here:\n"
        ,"- Technical [articles articles/index.html] or texts that I wrote\n"
        ,"- [Debian debian/index.html]-related stuff and software.\n"
        ,"- Information [about me aboutme/index.html]\n"
        ,"\n\n"]
    hPutStr h "= Whatsnew =\n\n\n"
    mapM_ (shownews h) news
    hPutStr h "\n\n\n"


shownews :: Handle -> (Day, String) -> IO ()
shownews h (d, n) = do
    hPutStr h $ "- **" ++ show d ++ "**\n\n"
    hPutStr h $ "  " ++ n ++ "\n\n"


main :: IO ()
main = do
    [dst] <- getArgs
    withFile dst WriteMode indexbuild





