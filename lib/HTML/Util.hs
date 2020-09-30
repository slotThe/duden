{- |
   Module      : HTML.Util
   Description : Utility functions for parsing and handling HTML
   Copyright   : (c) slotThe, 2020
   License     : AGPL
   Maintainer  : slotThe <soliditsallgood@mailbox.org>
   Stability   : experimental
   Portability : non-portable

Some of this is stolen from the 'tagsoup' package, because 'parse-html' is
(while faster) somewhat bare-bones.
-}
module HTML.Util
  ( -- * Making requests
    getTags            -- :: Request -> Manager -> IO [Token]
  , makeRequestWith    -- :: (String -> String) -> String -> IO Request

    -- * Taking parts of a list of 'Token's
  , sections           -- :: (a -> Bool) -> [a] -> [[a]]
  , section            -- :: (a -> Bool) -> [a] -> [a]
  , toHeadContentText  -- :: [Token] -> Text
  , between            -- :: String -> String -> [Token] -> [Token]
  , betweenTupleVal    -- :: Token -> [Token] -> [Text]
  , dropHeader         -- :: String -> [Token] -> [Token]
  , allContentText     -- :: [Token] -> [Text]

    -- * Filtering 'Token's
  , (~==)              -- :: Token -> Token -> Bool
  , (~/=)              -- :: Token -> Token -> Bool
  , isContentText      -- :: Token -> Bool

    -- * Creating 'Token's
  , divTag             -- :: Text -> Token
  , infoTag            -- :: Text -> Token

    -- * Misc
  , fromContentText    -- :: Token -> Text
  , cleanWord          -- :: String -> String
  , notNullWith        -- :: Foldable t => (t a -> a) -> t a -> Maybe a
  , notNull            -- :: Monoid a => [a] -> Maybe a
  ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List.NonEmpty         as NE
import qualified Data.Text as T

import Network.HTTP.Conduit (Manager, Request, httpLbs, parseRequest, responseBody)
import Text.HTML.Parser (Attr(Attr), AttrName, AttrValue, Token(Comment, ContentText, TagClose, TagOpen), parseTokens)

-- | Make a request and parse the response body into 'Token's.
getTags :: Manager -> Request -> IO [Token]
getTags man req =
  httpLbs req man <&> responseBody .> BL.toStrict .> decodeUtf8 .> parseTokens

-- | Build a URL and then make a request with that.
makeRequestWith :: (String -> String) -> String -> IO Request
makeRequestWith intoURL = cleanWord .> intoURL .> parseRequest

-- | This function takes a list, and returns all suffixes whose first item
-- matches the predicate.
sections :: (a -> Bool) -> [a] -> [[a]]
sections p = tails .> init .> filter (head .> p)

-- | Like 'sections', but return the head element (if present).
section :: forall a. (a -> Bool) -> [a] -> [a]
section f = \case
  [] -> []
  xs -> if null sns then [] else head sns
   where sns :: [[a]] = sections f xs

-- | Test if a 'Token' is a 'ContentText'.
isContentText :: Token -> Bool
isContentText = \case
    ContentText{} -> True
    _             -> False

-- | Extract the string from within 'ContentText', crashes if not a
-- 'ContentText'.
fromContentText :: Token -> Text
fromContentText = \case
    ContentText t -> t
    a             -> error $ "(" ++ show a ++ ") is not a ContentText"

-- | Get the first 'ContentText' element from a list of 'Token's.  If no tag
-- could be found, return an empty string.
toHeadContentText :: [Token] -> Text
toHeadContentText =
  filter isContentText .> nonEmpty .> maybe "" (NE.head .> fromContentText)

-- | Get all tags between @start@ and @end@.
between :: Token -> Token -> [Token] -> [Token]
between start end = dropWhile (~/= start)
                 .> drop 1                 -- drop the tag
                 .> takeWhile (~/= end)

-- | Get the first 'ContentText' out of some tuple__val tag.
betweenTupleVal :: Token -> [Token] -> [Text]
betweenTupleVal tag tags
   =  sections (~== tag) tags
  <&> between (TagOpen "dd" [Attr "class" "tuple__val"]) (TagClose "dd")
   .> toHeadContentText

-- | Drop an HTML header (i.e. the header tags and everything inbetween) from a
-- list of 'Token's.
dropHeader :: Text -> [Token] -> [Token]
dropHeader hname = dropWhile (~/= TagOpen "header" [Attr "class" hname])
                .> dropWhile (~/= TagClose "header")
                .> drop 1

-- | Get all 'ContentText' entries from a list of 'Token's and extract their
-- content.
allContentText :: [Token] -> [Text]
allContentText = filter isContentText .> map fromContentText

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

infixl 9 ~==
-- | Performs an inexact match, the first item should be the thing to match.
(~==) :: Token -> Token -> Bool
(~==) a b = f a b
 where
  f (ContentText y) (ContentText x) = T.null x             || x == y
  f (TagClose    y) (TagClose    x) = T.null x             || x == y
  f (Comment     x) (Comment     y) = x == mempty          || x == y
  f (TagOpen  y ys) (TagOpen  x xs) = (T.null x || x == y) && all g xs
   where
    g :: Attr -> Bool
    g = \case
      Attr name val | T.null name -> val  `elem` map attrSnd ys
                    | T.null val  -> name `elem` map attrFst ys
      nameval      -> nameval `elem` ys
    attrFst :: Attr -> AttrName  = \(Attr o _) -> o
    attrSnd :: Attr -> AttrValue = \(Attr _ t) -> t
  f _ _ = False

infixl 9 ~/=
-- | Negation of '(~==)'.
(~/=) :: Token -> Token -> Bool
(~/=) = (not .) . (~==)
