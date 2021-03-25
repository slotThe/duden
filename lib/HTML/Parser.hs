{- |
   Module      : HTML.Parser
   Description : Parsing HTML
   Copyright   : (c) slotThe  2020 2021
   License     : AGPL
   Maintainer  : slotThe <soliditsallgood@mailbox.org>
   Stability   : experimental
   Portability : non-portable

Here we actually scrape the HMTL that interests us.  Due to the very nature of
HTML scraping, this is very much written in more of a "it's not pretty, but it
works for now and it's pretty easy to rewrite" kind of way.
-}
module HTML.Parser
  ( searchForWord  -- :: Manager -> String -> IO [String]
  , lookupWord     -- :: Manager -> [Section] -> Natural -> String -> IO Text
  ) where

import qualified Data.Text as T

import Data.Map.Strict (Map)
import HTML.Types (DudenWord (DudenWord, meaning, name, synonyms, usage, wordClass), Section, WordMeaning (Multiple, Single), ppWord)
import HTML.Util (betweenTupleVal, divTag, getTags, infoTag, makeRequestWith, notNull)
import Network.HTTP.Conduit (Manager, parseRequest)
import Text.HTML.Parser (Attr (Attr), Token (TagClose, TagOpen))
import Text.HTML.Parser.Utils (allContentText, between, dropHeader, fromContentText, isContentText, section, sections, (~==))


-- | Search for the word on the Duden website.
searchForWord :: Manager -> String -> IO [String]
searchForWord man word =
  catch (parseRequest (wordSearch word)
           >>= getTags man
           <&> getWords .> map T.unpack)
        \(e :: SomeException) -> pure [show e]
 where
  -- | Return the search results as a list of values (i.e. each element in the
  -- list will be one word to later look up).
  getWords :: [Token] -> [Text]
  getWords tags = if null ws then ["Nichts gefunden :("] else ws
   where
    ws :: [Text]
        = sections (~== TagOpen "h2" [Attr "class" "vignette__title"]) tags
      <&> filter isContentText
       .> (!! 2)                -- ouch
       .> fromContentText
       .> T.filter (/= '\173')  -- "-"

-- | Look up the entry for a word on the Duden website.
lookupWord :: Manager -> [Section] -> Natural -> String -> IO Text
lookupWord man sns wrap word =
  catch do tags <- makeRequestWith wordPage word >>= getTags man
           ppWord DudenWord{ name      = fromString   word
                           , meaning   = getMeaning   tags
                           , usage     = getUsage     tags
                           , wordClass = getWordClass tags
                           , synonyms  = getSynonyms  tags
                           }
                  wrap
                  sns
            & pure
        \(e :: SomeException) -> e & tshow .> pure

-- | Try to get all of the synonyms (Synonyme) of a word.
getSynonyms :: [Token] -> Maybe Text
getSynonyms
   = section (~== divTag "synonyme")
  .> dropHeader [Attr "class" "division__header"]
  .> between (TagOpen "ul" []) (TagClose "ul")
  .> allContentText
  .> notNull

-- | Try to got the word class (Wortart) of a word.
getWordClass :: [Token] -> Maybe Text
getWordClass = betweenTupleVal (infoTag "wortart") .> notNull

-- | Try to get the usage (Gebrauch) of a word.
getUsage :: [Token] -> Maybe Text
getUsage tags = notNull (betweenTupleVal (infoTag "gebrauch") tags)

-- | Try to get the meaning (Bedeutung) of a word.  This may be a single meaning
-- or multiple ones.
getMeaning :: [Token] -> Maybe WordMeaning
getMeaning tags
  | notEmpty multiMeanings        = multipleMeanings & Multiple .> Just
  | singleMeaning & T.null .> not = singleMeaning    & Single   .> Just
  | otherwise                     = Nothing
 where
  singleMeaning :: Text
     = tags
     & section (~== meaning)
    .> between (TagOpen "p" []) (TagClose "p")
    .> allContentText
    .> mconcat

  multipleMeanings :: Map Int [Text]
     = tags
     & section (~== meanings)
    .> sections (~== enumItem)
    .> map (between enumItem enumItem
            .> sections (~== meaningsText)
            .> map getText)
    .> zip [1 ..]
    .> fromList

  multiMeanings :: [Text]
     = tags
     & section (~== meanings) .> sections (~== meaningsText)
    .> map getText

  getText :: [Token] -> Text
    = between meaningsText (TagClose "div") .> allContentText .> mconcat

  notEmpty :: [a] -> Bool
  notEmpty = null .> not

  meaning      :: Token = divTag "bedeutung"
  meanings     :: Token = divTag "bedeutungen"
  meaningsText :: Token = TagOpen "div" [Attr "class" "enumeration__text"]
  enumItem     :: Token = TagOpen "li"  [Attr "class" "enumeration__item"]

-- | Searching for a word.
wordSearch :: String -> String
wordSearch = ("https://www.duden.de/suchen/dudenonline/" <>)

-- | Looking up a word directly.
wordPage :: String -> String
wordPage = ("https://www.duden.de/rechtschreibung/" <>)

-- -- | This is nice for debugging.
-- htmlForWord :: Manager -> String -> IO ()
-- htmlForWord man word = do
--   req <- makeRequestWith wordPage word
--   httpLbs req man >>=
--     responseBody .> BL.toStrict .> decodeUtf8 .> writeFile (word ++ ".htm")
