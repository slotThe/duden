{- |
   Module      : HTML.Util
   Description : Utility functions for parsing and handling HTML
   Copyright   : (c) slotThe, 2020
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
  ) where

import qualified Data.ByteString.Lazy.Char8 as BL

import Network.HTTP.Conduit (Manager, Request, httpLbs, parseRequest, responseBody)
import Text.HTML.Parser (Attr(Attr), Token(TagClose, TagOpen), parseTokens)
import Text.HTML.Parser.Utils ((~==), between, sections, toHeadContentText)


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
infoTag t = TagOpen "a" [ Attr "target" "_blank"
                        , Attr "class"  "info-ref"
                        , Attr "href"   ("/hilfe/" <> t)
                        ]
