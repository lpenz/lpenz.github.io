module Main where

import System.FilePath
import Data.Time.Calendar
import System
import Text.XML.Light as XML
import Data.List.Utils

import RenderLib


-- Articles: ------------------------------------

getArticles :: [FilePath] -> IO ([(Day, (FilePath, (String, String)))])
getArticles anames = do
    at2t  <- mapM readFile anames
    ahtml0 <- mapM t2tfileToHtml anames
    let ahtml = map ( \ (f, c) -> replace "$cwd$" (home ++ "/" ++ init (dropFileName f)) c ) $ zip anames ahtml0
    let alines = map lines at2t
    let titles = map (!! 0) alines
    let dates  = map (!! 2) alines
    return $ zip (map read dates) $ zip anames $ zip titles ahtml

-- Feed: ----------------------------------------

feedbuild :: [(Day, (FilePath, (String, String)))] -> XML.Element
feedbuild newshtml = rss {
    elAttribs = [
        Attr (qualName "version") "2.0"
        ,Attr (qualName "xmlns:atom") "http://www.w3.org/2005/Atom" ] }
    where
        title = xmlLeaf "title" "Avulsos by Penz - Articles"
        link = xmlLeaf "link" home
        rss =
            qualNode "rss" $ (:[]) $ Elem
            $ qualNode "channel" $ map Elem $ [
                title
                ,link
                ,xmlLeaf "description" "Articles in Avulsos by Penz page."
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
                    Attr  (qualName "href") $ home ++ "/articles.xml"
                    ,Attr (qualName "rel")  "self"
                    ,Attr (qualName "type") "application/rss+xml"] }
                    ]
              ++ map feeditems newshtml
        maxdate = foldr1 max $ map fst newshtml


feeditems :: (Day, (FilePath, (String, String))) -> XML.Element
feeditems (d, (n, (t, s))) =
    qualNode "item" $ map Elem
    $ [
        xmlLeaf  "title" $ t
        ,xmlLeaf "link" $ home ++ "/" ++ name
        ,xmlLeaf "guid" $ home ++ "/" ++ name
        ,xmlLeaf "pubDate" (formatdayrfc d)
        ,xmlHtml "description" $ replace "$home$" home s
        ]
    where name = replace ".t2t" ".html" n



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
    (feed:anames) <- getArgs
    articles <- getArticles anames
    writeFile feed (showTopElement $ feedbuild articles)


