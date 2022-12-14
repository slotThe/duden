{- |
   Module      : HTML.Types
   Description : Types for the parts of a Duden entry that interest us
   Copyright   : (c) slotThe  2020 2021
   License     : AGPL
   Maintainer  : slotThe <soliditsallgood@mailbox.org>
   Stability   : experimental
   Portability : non-portable

Here we define the types we scrape the appropriate info into.  In addition, we
define sections to be printed.  This way the user can later specify exactly the
kind of information they want to see.

We also define a pretty printing function to fit all of this together.
-}
module HTML.Types
  ( -- * Scraping HTML into these types
    DudenWord(..)
  , WordMeaning(..)

    -- * Showable things when pretty printing
  , Section(..)      -- instances: Eq, Show

    -- * Pretty printing
  , ppWord           -- :: DudenWord -> Natural -> [Section] -> Text
  ) where

import CmdLine.Util (wrapText)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T


-- | Entry for a single word where not all sections have to be present.
data DudenWord = DudenWord
  { name      :: !Text
  , wordClass :: !(Maybe Text)
  , usage     :: !(Maybe Text)
  , meaning   :: !(Maybe WordMeaning)
  , synonyms  :: !(Maybe Text)
  }

-- | A word may have multiple meanings.
data WordMeaning
  = Single   !Text
  | Multiple !(Map Int [Text])

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
ppWord :: DudenWord -> Natural -> [Section] -> Text
ppWord dw@DudenWord{ name } wrap =
  map (ppSection dw wrap) .> catMaybes .> (wordName :) .> unlines
 where
  wordName :: Text = mconcat
    [ style 1 name  -- 1 = bold
    , "\n"
    , T.replicate (if wrap == 0 then 79 else fi wrap) "-"
    ]

-- | Given a word entry, pretty print a single 'Section' (if present).
ppSection :: DudenWord -> Natural -> Section -> Maybe Text
ppSection DudenWord{ meaning, usage, wordClass, synonyms } wrap = \case
  WordClass -> wordClass <&> pp WordClass
  Usage     -> usage     <&> pp Usage
  Meaning   -> meaning   <&> pp Meaning . ppMeaning
  Synonyms  -> synonyms  <&> wrapSynonyms .> pp Synonyms
 where
  wrapSynonyms :: Text -> Text
     = T.splitOn ","
    .> map T.strip
    .> wrapText ", " (T.length (tshow Synonyms)) (fi wrap)

  ppMeaning :: WordMeaning -> Text
  ppMeaning = \case
    Single   t  -> ": " <> t
    Multiple ts -> (ts <&> \t -> ppMultiple T.singleton ") " ['a' ..] t
                               & align 6)
                 & enumerate
                 & ("en: " <>)

  -- | Pair a list with its indices and format everything in a nice way.
  enumerate :: Map Int Text -> Text
    = Map.foldrWithKey (\k v l -> mconcat ["\n  ", tshow k, ".  ", v, l]) ""

  -- | Pretty-print multiple meanings.
  ppMultiple :: (a -> Text) -> Text -> [a] -> [Text] -> [Text]
  ppMultiple showN str ixs lst
    | length lst <= 1 = lst
    | otherwise       = zipWith (\n line -> showN n <> str <> line) ixs lst

  -- | Align a sub-text; this means aligning everything but the first
  -- item, as it'll be on the same line as its parent.
  align :: Int -> [Text] -> Text
  align n = \case
    []       -> ""
    [x]      -> wrapAt 6 x
    (x : xs) -> wrapAt 9 x <> go xs
   where
    wrapAt :: Int -> Text -> Text
    wrapAt k = wrapText " " k (fi wrap) . T.words

    go :: [Text] -> Text = \case
      []       -> ""
      (y : ys) -> "\n" <> T.replicate n " " <>
                wrapText " " 9 (fi wrap) (T.words y)
                <> go ys

  {- | Slap a 'Section' in front of some 'Text', then pretty print the first
     word (i.e. everything until the first space character) in some nice amber
     colour.
  -}
  pp :: Section -> Text -> Text
  pp s = (tshow s <>) .> T.breakOn " " .> first (style 33) .> concatTuples
   where
    concatTuples :: Semigroup a => (a, a) -> a
    concatTuples (a, b) = a <> b

-- | See https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_parameters
style :: Int -> Text -> Text
style i s = mconcat ["\x1b[", tshow i, "m"
                    , s
                    , "\x1b[0m"
                    ]
