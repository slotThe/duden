{- |
   Module      : HTML.Parser
   Description : Parsing HTML
   Copyright   : (c) slotThe, 2020
   License     : AGPL
   Maintainer  : slotThe <soliditsallgood@mailbox.org>
   Stability   : experimental
   Portability : non-portable
-}
module HTML.Parser
  ( lookForWords  -- :: Manager -> String -> IO [String]
  , lookupWord    -- :: Manager -> [Section] -> String -> IO Text
  ) where

import HTML.Types (DudenWord(DudenWord, meaning, name, synonyms, usage, wordClass), Section, WordMeaning(Multiple, Single), ppWord)
import HTML.Util ((~==), allContentText, between, divTag, dropHeader, fromContentText, getTags, infoTag, isContentText, makeRequestWith, notNull, section, sections, toHeadContentText)

import qualified Data.Text as T

import Network.HTTP.Conduit (Manager, parseRequest)
import Text.HTML.Parser (Attr(Attr), Token(TagClose, TagOpen))


lookForWords :: Manager -> String -> IO [String]
lookForWords man word =
  catch (parseRequest (wordSearch word) >>= getTags man
                                        <&> getWords .> map T.unpack)
        \(e :: SomeException) -> pure [show e]
 where
  getWords :: [Token] -> [Text]
  getWords tags = if null ws then ["Nichts gefunden :("] else ws
   where
    ws :: [Text]
       =  sections (~== TagOpen "h2" [Attr "class" "vignette__title"]) tags
      <&> filter isContentText
       .> (!! 2)                -- ouch
       .> fromContentText
       .> T.filter (/= '\173')

lookupWord :: Manager -> [Section] -> String -> IO Text
lookupWord man sns word =
  catch do tags <- makeRequestWith wordPage word >>= getTags man
           pure $ ppWord DudenWord{ name      = T.pack word
                                  , meaning   = getMeaning   tags
                                  , usage     = getUsage     tags
                                  , wordClass = getWordClass tags
                                  , synonyms  = getSynonyms  tags
                                  }
                         sns
        \(e :: SomeException) -> pure (tshow e)

getSynonyms :: [Token] -> Maybe Text
getSynonyms
   = section (~== divTag "synonyme")
  .> dropHeader "division__header"
  .> between (TagOpen "ul" []) (TagClose "ul")
  .> allContentText
  .> notNull

getWordClass :: [Token] -> Maybe Text
getWordClass = betweenTupleVal (infoTag "wortart") .> notNull

-- | Sometimes no usage information is given.
getUsage :: [Token] -> Maybe Text
getUsage tags = notNull (betweenTupleVal (infoTag "gebrauch") tags)

betweenTupleVal :: Token -> [Token] -> [Text]
betweenTupleVal tag tags
   =  sections (~== tag) tags
  <&> between (TagOpen "dd" [Attr "class" "tuple__val"]) (TagClose "dd")
   .> toHeadContentText

getMeaning :: [Token] -> WordMeaning
getMeaning tags
  | T.null singleMeaning = Multiple multipleMeanings
  | otherwise            = Single   singleMeaning
 where
  singleMeaning :: Text
     = tags
     & section (~== meaning)
    .> between (TagOpen "p" []) (TagClose "p")
    .> toHeadContentText

  multipleMeanings :: [Text]
     = tags
     & section  (~== meanings)
    .> sections (~== meaningsText)
    .> map (between meaningsText (TagClose "div") .> allContentText .> mconcat)
    .> reverse  -- Same order as on the website.

  meaning, meanings :: Token
  meaning  = divTag "bedeutung"
  meanings = divTag "bedeutungen"
  meaningsText :: Token = TagOpen "div" [Attr "class" "enumeration__text"]

wordSearch :: String -> String
wordSearch = ("https://www.duden.de/suchen/dudenonline/" <>)

wordPage :: String -> String
wordPage = ("https://www.duden.de/rechtschreibung/" <>)

-- -- | This is nice for debugging.
-- htmlForWord :: Manager -> String -> IO ()
-- htmlForWord man word = do
--   req <- makeRequestWith wordPage word
--   httpLbs req man >>=
--     responseBody .> BL.toStrict .> decodeUtf8 .> T.writeFile (word ++ ".htm")
