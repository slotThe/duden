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


-- | Entry for a single word where not all sections have to be present.
data DudenWord = DudenWord
  { name      :: !Text
  , wordClass :: !(Maybe Text)
  , usage     :: !(Maybe Text)
  , meaning   :: !WordMeaning
  , synonyms  :: !(Maybe Text)
  }

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

-- | Pretty print the given 'Section's of a single word entry.
ppWord :: DudenWord -> [Section] -> Text
ppWord dw@DudenWord{ name }
   = map (ppSection dw)
  .> catMaybes
  .> (wordName :)
  .> T.unlines
 where
  wordName :: Text         = bold name <> "\n" <> T.replicate 79 "-"
  bold     :: Text -> Text = \s -> "\x1b[1m" <> s <> "\x1b[0m"

-- | Given a word entry, pretty print a single 'Section' (if present).
ppSection :: DudenWord -> Section -> Maybe Text
ppSection DudenWord{ meaning, usage, wordClass, synonyms } = \case
  WordClass -> wordClass <&> pp WordClass
  Usage     -> usage     <&> pp Usage
  Meaning   -> Just ppMeaning
  Synonyms  -> synonyms  <&> pp Synonyms
 where
  ppMeaning :: Text = pp Meaning $ case meaning of
    Single   t  -> style ": "   <> t
    Multiple ts -> style "en: " <> foldl' (\str t -> "\n  - " <> t <> str) "" ts

  -- | See https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_parameters
  style :: Text -> Text
  style s = "\x1b[33m" <> s <> "\x1b[0m"

  pp :: Section -> Text -> Text
  pp s = (style (tshow s) <>)
