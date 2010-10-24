module Main where

import Data.Time.Calendar
import System
import System.IO
import Text.Feed.Constructor
import Text.Feed.Export
import Text.Feed.Types
import Text.XML.Light.Output

import RenderLib


news :: [(Day, String)]
news = [
    ( fromGregorian 2010 10 23,
        "Main page now has a //whatsnew// section!")
    ,(fromGregorian 2010  8 15,
        "New [Data exploring with R: hard drive occupation prediction articles/df0pred-1/index.html] article.")
    ,(fromGregorian 2010  4 11,
        "New [Debianization with git-buildpackage articles/debgit/index.html] article.")
    ]


-- Page: ----------------------------------------

pagebuild :: Handle -> IO ()
pagebuild h = do
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
    mapM_ (pagenews h) news
    hPutStr h "\n\n\n"


pagenews :: Handle -> (Day, String) -> IO ()
pagenews h (d, n) = do
    hPutStr h $ "- **" ++ show d ++ "**\n\n"
    hPutStr h $ "  " ++ n ++ "\n\n"

-- Feed: ----------------------------------------

feedkind :: FeedKind
feedkind = RSSKind $ Just "2.0"


feedbuild :: [(Day, String)] -> String
feedbuild newshtml =
    showTopElement
    $ xmlFeed
    $ withFeedTitle "Avulsos by Penz's whatsnew"
    $ withFeedDescription "Feed for whatsnew section of Avulsos by Penz"
    $ withFeedHome "http://lpenz.github.com/"
    $ withFeedLanguage "en"
    $ withFeedGenerator ("Hand-made with haskell's Feed package", Nothing)
    $ withFeedDate (formatdayrfc maxday)
    $ withFeedItems items
    $ newFeed feedkind
    where
        maxday :: Day
        maxday = foldr1 max $ map fst news
        items :: [Item]
        items = map itemformat newshtml


itemformat :: (Day, String) -> Item
itemformat (d, s) =
    withItemTitle s
    $ withItemDate (formatdayrfc d)
    $ newItem feedkind

-- Main: ----------------------------------------

main :: IO ()
main = do
    [page, feed] <- getArgs
    withFile page WriteMode pagebuild
    newshtml <- mapM ( \ (d, s) -> t2tToHtml s >>= \ n -> return (d, n)) news
    writeFile feed (feedbuild newshtml)


