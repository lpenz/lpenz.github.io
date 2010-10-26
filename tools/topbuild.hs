module Main where

import Data.Time.Calendar
import System
import System.IO
import Text.XML.Light as XML
import Data.List.Utils

import RenderLib


news :: [(Day, String)]
news = reverse [
    ( fromGregorian 2010  4 11,
        "New [Debianization with git-buildpackage $home$/articles/debgit/index.html] article.")
    ,(fromGregorian 2010  8 15,
        "New [Data exploring with R: hard drive occupation prediction $home$/articles/df0pred-1/index.html] article.")
    ,(fromGregorian 2010 10 23,
        "Main page now has a //whatsnew// section!")
    ,(fromGregorian 2010 10 24,
        "Whatsnew feed up and running.")
    ,(fromGregorian 2010 10 25,
        "Articles feed up and running.")
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
    hPutStr h $ "== " ++ show d ++ " ==[" ++ show d ++ "]\n\n"
    hPutStr h $ "  " ++ replace "$home$" "" n ++ "\n\n"

-- Feed: ----------------------------------------

feedbuild :: [(Day, String)] -> XML.Element
feedbuild newshtml = rss {
    elAttribs = [
        Attr (qualName "version") "2.0"
        ,Attr (qualName "xmlns:atom") "http://www.w3.org/2005/Atom" ] }
    where
        title = xmlLeaf "title" "Avulsos by Penz - Whatsnew"
        link = xmlLeaf "link" home
        rss =
            qualNode "rss" $ (:[]) $ Elem
            $ qualNode "channel" $ map Elem $ [
                title
                ,link
                ,xmlLeaf "description" "Whatsnew in Avulsos by Penz page."
                ,xmlLeaf "managingEditor" "llpenz@gmail.com (Leandro Lisboa Penz)"
                ,xmlLeaf "webMaster" "llpenz@gmail.com (Leandro Lisboa Penz)"
                ,xmlLeaf "docs" "http://www.rssboard.org/rss-specification"
                ,xmlLeaf "pubDate" (formatdayrfc maxdate)
                ,xmlLeaf "lastBuildDate" (formatdayrfc maxdate)
                ,xmlLeaf "language" "en"
                ,qualNode "image" $ map Elem [
                    title
                    ,link
                    ,xmlLeaf "url" $ home ++ "/logo.png"
                    ]
                ,blank_element { elName = qualName "atom:link", elAttribs = [
                    Attr  (qualName "href") $ home ++ "/whatsnew.xml"
                    ,Attr (qualName "rel")  "self"
                    ,Attr (qualName "type") "application/rss+xml"] }
                    ]
              ++ map feeditems newshtml
        maxdate = foldr1 max $ map fst newshtml


feeditems :: (Day, String) -> XML.Element
feeditems (d, s) =
    qualNode "item" $ map Elem
    $ [
        xmlLeaf  "title" $ "News for " ++ show d
        ,xmlLeaf "link" $ home ++ "/index.html#" ++ show d
        ,xmlLeaf "guid" $ home ++ "/index.html#" ++ show d
        ,xmlLeaf "pubDate" (formatdayrfc d)
        ,xmlHtml "description" $ replace "$home$" home s
        ]



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


