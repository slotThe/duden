{- |
   Module      : HTML.Util
   Description : Utility functions for parsing and handling HTML
   Copyright   : (c) slotThe  2020 2021
   License     : AGPL
   Maintainer  : slotThe <soliditsallgood@mailbox.org>
   Stability   : experimental
   Portability : non-portable
-}
module HTML.Util
  ( -- * Making requests
    getTags            -- :: Request -> Manager -> IO [Token]
  , makeRequestWith    -- :: (String -> String) -> String -> IO Request

    -- * Taking parts of a list of 'Token's
  , betweenTupleVal    -- :: Token -> [Token] -> [Text]

    -- * Creating 'Token's
  , divTag             -- :: Text -> Token
  , infoTag            -- :: Text -> Token

    -- * Misc
  , cleanWord          -- :: String -> String
  , notNullWith        -- :: Foldable t => (t a -> a) -> t a -> Maybe a
  , notNull            -- :: Monoid a => [a] -> Maybe a
  , wrapWith           -- :: Text -> Int -> Int -> [Text] -> Text
  ) where

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client (Manager, Request, httpLbs, parseRequest, responseBody)
import Text.HTML.Parser (Attr (Attr), Token (TagClose, TagOpen), parseTokens)
import Text.HTML.Parser.Util (between, sections, toHeadContentText, (~==))


-- | Make a request and parse the response body into 'Token's.
getTags :: Manager -> Request -> IO [Token]
getTags man req =
  httpLbs req man <&> responseBody .> BL.toStrict .> decodeUtf8 .> parseTokens

-- | Build a URL and then make a request with that.
makeRequestWith :: (String -> String) -> String -> IO Request
makeRequestWith intoURL = cleanWord .> intoURL .> parseRequest

-- | Get the first 'ContentText' out of some tuple__val tag.
betweenTupleVal :: Token -> [Token] -> [Text]
betweenTupleVal tag tags
   =  sections (~== tag) tags
  <&> between (TagOpen "dd" [Attr "class" "tuple__val"]) (TagClose "dd")
   .> toHeadContentText

-- | Word pages are accessed with ASCII only, so we need to replace Umlauts for
-- certain lookups.
cleanWord :: String -> String
cleanWord = concatMap \case
  'ä' -> "ae"
  'Ä' -> "Ae"
  'ü' -> "ue"
  'Ü' -> "Ue"
  'ö' -> "oe"
  'Ö' -> "oe"
  'ß' -> "ss"
  c   -> [c]

-- | Like 'notNullWith', but just throw everything together with an 'mconcat' at
-- the end.
notNull :: Monoid a => [a] -> Maybe a
notNull = notNullWith mconcat
{-# INLINE notNull #-}

-- | Check if some container is null; if not then apply some function to it.
notNullWith :: Foldable t => (t a -> a) -> t a -> Maybe a
notNullWith f t = if null t then Nothing else Just (f t)
{-# INLINE notNullWith #-}

-- | This kind of tag is often used at the start of a certain section.
divTag :: Text -> Token
divTag t = TagOpen "div" [Attr "class" "division ", Attr "id" t]

-- | This kind of tag is often used at the start of a certain section.
infoTag :: Text -> Token
infoTag t = TagOpen "a" [ Attr "class" "tuple__icon"
                        , Attr "href"  ("/hilfe/" <> t)
                        ]

-- | Wrap text at @N@ columns.
wrapWith
  :: Text    -- ^ How to concatenate chunks, i.e. the separator
  -> Int     -- ^ Left alignment
  -> Int     -- ^ Max line length (wrap)
  -> [Text]  -- ^ Text as chunks that have to stay together
  -> Text    -- ^ Text with line breaks
wrapWith separator al wrapAt chunks
  | wrapAt == 0 = mconcat $ intersperse separator chunks
  | otherwise   = decodeUtf8 $ go "" (encodeUtf8 separator) al (map encodeUtf8 chunks)
 where
  go :: ByteString  -- ^ Already processed part of the text
     -> ByteString  -- ^ Separator to put between chunks
     -> Int         -- ^ Counter of the current line length
     -> [ByteString]  -- ^ Text as chunks that have to stay together
     -> ByteString
  go !done _   !_   []        = done
  go !line sep !acc xs@(c:cs)
    | cLen      >= wrapAt = go goAgain                  sep newLen cs
    | al + cLen >= wrapAt = go (goAgain <> ", ")        sep newLen cs
    | combLen   >= wrapAt = go (align line)             sep al     xs
    | otherwise           = go (mconcat [line, c, end]) sep newLen cs
   where
    goAgain :: ByteString = go line " " acc (BS.words c)

    cLen    :: Int = BS.length c
    combLen :: Int = acc + cLen               -- Length including the next word
    newLen  :: Int = combLen + BS.length end  -- Take separator length into account

    -- | Nicely left-align the text after a line-break.  We like
    -- pretty things.
    align :: ByteString -> ByteString
    align = (<> "\n" <> BS.replicate al ' ')

    end :: ByteString
    end = if null cs then "" else sep
