{- |
   Module      : HTML.Types
   Description : Types for the parts of a Duden entry that interest us
   Copyright   : (c) slotThe, 2020
   License     : AGPL
   Maintainer  : slotThe <soliditsallgood@mailbox.org>
   Stability   : experimental
   Portability : non-portable
-}
module HTML.Types
  ( DudenWord(..)    -- instances: Generic
  , WordMeaning(..)
  , Section(..)      -- instances: Eq, Show
  , ppWord           -- :: DudenWord -> [Section] -> Text
  ) where

import qualified Data.Text as T

import Data.Generics.Labels ()


-- | Entry for a single word.
data DudenWord = DudenWord
  { name      :: !Text
  , wordClass :: !(Maybe Text)
  , usage     :: !(Maybe Text)
  , meaning   :: !WordMeaning
  , synonyms  :: !(Maybe Text)
  } deriving (Generic)

-- | A word may have multiple meanings.
data WordMeaning
  = Single   !Text
  | Multiple ![Text]

-- | Sections to show in the final output.
data Section
  = Meaning
  | Usage
  | Synonyms
  | WordClass
  deriving (Eq)

instance Show Section where
  show :: Section -> String
  show = \case
    WordClass -> "Wortart: "
    Usage     -> "Gebrauch: "
    Meaning   -> "Bedeutung"
    Synonyms  -> "Synonyme: "

ppWord :: DudenWord -> [Section] -> Text
ppWord dw = map (ppSection (removeNewlines dw))
         .> catMaybes
         .> (wordName :)
         .> T.unlines
 where
  wordName :: Text = bold (dw ^. #name) <> "\n" <> T.replicate 79 "-"
  bold :: Text -> Text
  bold s = "\x1b[1m"  <> s <> "\x1b[0m"

ppSection :: DudenWord -> Section -> Maybe Text
ppSection DudenWord{ meaning, usage, wordClass, synonyms } = \case
  WordClass -> wordClass <&> pp WordClass
  Usage     -> usage     <&> pp Usage
  Meaning   -> Just ppMeaning
  Synonyms  -> synonyms  <&> pp Synonyms
 where
  ppMeaning :: Text = style (tshow Meaning) <> case meaning of
    Single   t  -> style (": ")   <> t
    Multiple ts -> style ("en: ") <> foldl' (\str t -> "\n  - " <> t <> str) "" ts

  -- | See https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_parameters
  style :: Text -> Text
  style s = "\x1b[33m" <> s <> "\x1b[0m"

  pp :: Section -> Text -> Text
  pp s = (style (tshow s) <>)

removeNewlines :: DudenWord -> DudenWord
removeNewlines dw =
  dw & #name       %~  f
     & #wordClass  %~~ f
     & #usage      %~~ f
     & #meaning    %~  \case
         Single   t  -> Single   (t   &  f)
         Multiple ts -> Multiple (ts <&> f)
     & #synonyms   %~~ f
 where
  f :: Text -> Text = T.filter (/= '\n')
