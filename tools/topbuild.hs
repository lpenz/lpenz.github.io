module Main where

import Data.Time.Calendar
import System
import System.IO
import Text.XML.Light as XML

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

feedbuild :: [(Day, String)] -> XML.Element
feedbuild newshtml =
    qualNode "rss" $ (:[]) $ Elem
    $ qualNode "channel" $ map Elem
    $ [xmlLeaf "title" "Avulsos by Penz - Whatsnew"
        ,xmlLeaf "link" "http://lpenz.github.com/"
        ,xmlLeaf "description" "Whatsnew section of Avulsos by Penz page."]
      ++ map feeditems newshtml


feeditems :: (Day, String) -> XML.Element
feeditems (d, s) =
    qualNode "item" $ map Elem
    $ [xmlHtml "description" s
        ,xmlLeaf "pubDate" (formatdayrfc d)]


qualNode :: String -> [XML.Content] -> XML.Element
qualNode n cs = blank_element { elName = qualName n , elContent = cs }

qualName :: String -> QName
qualName n = QName{qName=n,qURI=Nothing,qPrefix=Nothing}

xmlAttr :: String -> String -> XML.Attr
xmlAttr k v = Attr (qualName k) v

xmlLeaf :: String -> String -> XML.Element
xmlLeaf tg txt = blank_element{ elName = qualName tg , elContent = [ Text blank_cdata { cdData = txt } ] }

xmlHtml :: String -> String -> XML.Element
xmlHtml tg txt = blank_element{ elName = qualName tg , elContent = [ Text blank_cdata { cdVerbatim = CDataVerbatim, cdData = txt } ] }


-- Main: ----------------------------------------

main :: IO ()
main = do
    [page, feed] <- getArgs
    withFile page WriteMode pagebuild
    newshtml <- mapM ( \ (d, s) -> t2tToHtml s >>= \ n -> return (d, n)) news
    --writeFile feed (show $ feedbuild newshtml)
    writeFile feed (showTopElement $ feedbuild newshtml)


