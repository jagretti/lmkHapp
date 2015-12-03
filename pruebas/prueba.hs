
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
import Text.XML.HXT.Core
import Text.Printf
import Network.URI
import Network.HTTP
import Data.List
import Data.Char
import System.Environment
import Control.Monad (forM_)

-- Search Google and retrieve the results into some
-- nicer data structure.  This is an exercise in
-- munging the HTML output of Google.

-- A number of possible functions are demonstrated,
-- to show some different ways of going about the task,
-- only one is really needed.

-- Personally, I favor the 2nd version, selectGoogleResultsToXML.  
-- I think this is a good demonstration of a case where the 
-- ordinary Haskell combinators are clearer than the special 
-- Arrow syntax.

--------------------------------------------------
-- First example: Select pairs of (url, title)

-- There isn't any help, from Google, in picking out the results,
-- so I've poked around and came up with this:
-- look for 
--   <h3> ... <a href="..." ...>
-- and the results will be contained within the anchor.

{-
selectGoogleResults = 
  atTagCase "h3"
  >>> atTagCase "a"
  >>> (getAttrValue "href" &&& 
       -- The title may be broken up into multiple text nodes.
       -- So, we collect it as a list and then lift 'concat' 
       -- to combine it.
       (listA (deep isText >>> getText) >>> arr concat))

-- Or, select the data into a simpler XML document

selectGoogleResultsToXML = 
  selem "results" 
    [ atTagCase "h3"
      >>> atTagCase "a"
      >>> selem "result" 
            [ selem "url" [getAttrValue "href" >>> mkText]
            , selem "title" [deep isText] ] ] 
-}
-- The above function shows construction of XML,
-- 
-- selem tag body = mkelem tag [] body
--   constructs an element without attributes.
--
-- Note that 'body' is a list containing XmlTree arrows,
-- that is why you can mix selection and construction.

--------------------------------------------------
-- Alternative versions, using the special Arrow 
-- syntax.

selectGoogleResults' = 
  atTagCase "h3"
  >>> atTagCase "a"
  >>> proc r -> do
        url   <- getAttrValue "href"             -< r
        title <- listA (getText <<< deep isText) -< r
        returnA -< (url, concat title)

selectGoogleResultsToXML' = proc x -> do
  res <- listA (atTagCase "h3"
                >>> atTagCase "a"
                >>> selectResult) -< x
  selem "results" (map constA res) -<< ()
  where
    selectResult = proc r -> do
      url   <- getAttrValue "href"             -< r
      title <- listA (getText <<< deep isText) -< r
      selem "result" 
        [ selem "url" [txt url]
        , selem "title" [txt (concat title)] ]  -<< ()

-- This last example introduces some new syntax,
-- namely -<<.  According to the GHC Arrow docs,
-- you cannot use locally bound variables on the
-- left-hand side of -<.  In brief, the -<< syntax is
-- a variation which permits this, while introducing
-- a dependency on the ArrowApply class.

-- The reason why 'selem' is on the LHS in the
-- first place is because, if you check the type
-- of 'selem', you will see that it is already
-- in the Arrow (so to speak).  Injecting it
-- with 'returnA' would simply create an extra
-- layer of Arrow that is not wanted.

-- Since 'selem' is not processing anything, the RHS 
-- of -<< in this case is simply ().

googleURLFormat = "http://www.google.com/search?%s"
constructGoogleURL q = printf googleURLFormat $ urlEncodeVars [("q",q)]

-- case-insensitive tag matching
atTagCase tag = deep (isElem >>> hasNameWith ((== tag') . upper . localPart))
  where tag' = upper tag
        upper = map toUpper

parseHTML = readString [ withValidate no
                       , withParseHTML yes
                       , withWarnings no
                       ]

-- Pretend to be a user of Mozilla Firefox, because Google
-- will not display results for unknown user agents.

userAgent = "Mozilla/5.0 (en-US) Firefox/2.0.0.6667" 

get :: URI -> IO String
get uri = do
  let req = Request uri GET [] ""
  eresp <- simpleHTTP $ insertHeader HdrUserAgent userAgent req
  case eresp of
    Left er -> error $ show er
    Right res -> return $ rspBody res

main = do
  args <- getArgs
  case parseURI (constructGoogleURL (unwords args)) of
    Nothing -> putStrLn "Invalid search"
    Just uri -> do
      body  <- get uri

      -- tuple version
      links <- runX (parseHTML body >>> selectGoogleResults')
      forM_ links $ \ (url,title) -> printf "%s <url:%s>\n" title url
{-
      -- XML version
      [xml] <- runX (parseHTML body >>> 
                     -- At the top of a document is a hidden "root" node
                     -- which encompasses all the top-level siblings.
                     root [] [selectGoogleResultsToXML'] >>> 
                     writeDocumentToString [withIndent yes])
      putStrLn xml
-}
