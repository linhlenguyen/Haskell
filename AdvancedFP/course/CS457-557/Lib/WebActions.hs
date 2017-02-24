 {-# Language FlexibleInstances, ScopedTypeVariables #-}

-- if you can't get this to run, try: cabal install download-curl
-- we seem to have two different versions of type Tag
{-
*WebActions> :i TagOpen
data Tag str = TagOpen str [Attribute str] | ...
        -- Defined in `tagsoup-0.13.3:Text.HTML.TagSoup.Type'
*WebActions> :t getTags
getTags
  :: String -> IO [tagsoup-0.12.8:Text.HTML.TagSoup.Type.Tag String]
  
  I had to run ghcpkg hide tagsoup-0.13.3
  to get things to work.
-}


module WebActions(
    URL, urlName,
    getText,         -- :: URL -> IO String
    getByteString,   -- :: URL -> IO ByteString
    writeByteString, -- :: String -> ByteString -> IO ()
    downloadTo,      -- :: FilePath -> URL -> IO ()
    getTags,         -- :: URL -> IO [Tag]
    getHrefs,        -- :: URL -> IO [URL]
    getHTML,         -- :: URL -> IO [TagTree]
    getXML,          -- :: URL -> IO [Content]

  ) where

import Prelude -- hiding (catch)
import IOActions

-- use cabal install download-curl
import Network.Curl.Download

-- use     cabal install tagsoup    to get these modules
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import qualified Data.ByteString as BS


-- use     cabal install curl    to get this module
-- cabal install curl --extra-include-dirs=c:/curl/include --extra-lib-dirs=c:/curl/bin
-- import Network.Curl 
-- use cabal install xml
import Text.XML.Light -- hiding (Content)



import Treedot
import Data.Char
import Control.Exception -- hiding (catch)

type URL = String

-- Text: --------------------------------------------------------

getText    :: URL -> IO String
getText url = openURIString url >>= \(Right s) -> return s

-- ByteString: --------------------------------------------------

getByteString     :: URL -> IO BS.ByteString
getByteString url  = openURI url >>= \f ->
                     case f of
                       Right bs -> return bs
                       Left  er -> throw
                                 $ ErrorCall
                                 $ ("ERROR: url " ++ url ++", " ++ show er)

writeByteString   :: String -> BS.ByteString -> IO ()
writeByteString    = BS.writeFile


downloadTo        :: FilePath -> URL -> IO ()
downloadTo dir url = catch (getByteString url
                             >>= writeByteString (dir </> urlName url))
                           (\ (e::IOException) -> putStrLn ("Failed on " ++ url))

urlName           :: URL -> String
urlName            = reverse . takeWhile ('/'/=) . reverse

urlPrefix         :: URL -> String
urlPrefix          = reverse . dropWhile ('/'/=) . reverse

absoluteUrl       :: URL -> Bool
absoluteUrl url    = or (zipWith (\x y -> x=='/' && x==y) url (tail url))

relativeTo        :: URL -> URL -> URL
url `relativeTo` base
 | absoluteUrl url = url
 | otherwise       = urlPrefix base ++ url

-- Tags: --------------------------------------------------------

-- getTags     :: URL -> IO [Tag String]
getTags url  = openAsTags url >>= \(Right ts) -> return ts


getHrefs    :: URL -> IO [URL]
getHrefs url = getTags url >>= \ts ->
                return [ link `relativeTo` url
                       | (TagOpen t attrs) <- ts,
                         norm t == "a",
                         (tn, link) <- attrs,
                         norm tn == "href" ]
               where norm = map toLower

-- getHTML    :: URL -> IO [TagTree String]
getHTML url = getTags url >>= inIO tagTree



-- use tagTree :: [Tag] -> [TagTree]

instance Tree (TagTree String) where
  subtrees (TagBranch s as ts) = ts
  subtrees (TagLeaf l)         = []

instance LabeledTree (TagTree String) where
  label (TagBranch s as ts)   = s
  label (TagLeaf (TagText s)) = filter isPrint s
  label other                 = ""

-- XML: ---------------------------------------------------------

getXML    :: URL -> IO [Content]
getXML url = openAsXML url >>= \(Right c) -> return c

instance Tree Content where
  subtrees (Elem e) = elContent e
  subtrees other    = []

instance LabeledTree Content where
  label (Elem e) = qName (elName e)
  label (CRef s) = s
  label (Text t) = show (trunc (cdData t))
    where trunc t = case splitAt 12 t of
                      (as, []) -> as
                      (as, bs) -> as ++ "..."

-----------------------------------------------------------------
