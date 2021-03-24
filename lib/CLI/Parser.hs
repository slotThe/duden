{- |
   Module      : CLI.Parser
   Description : CLI for the program
   Copyright   : (c) slotThe  2020 2021
   License     : AGPL
   Maintainer  : slotThe <soliditsallgood@mailbox.org>
   Stability   : experimental
   Portability : non-portable
-}
module CLI.Parser
  ( Options(..)
  , options      -- :: ParserInfo Options
  ) where

import HTML.Types (Section (Meaning, Synonyms, Usage, WordClass))

import qualified Data.Attoparsec.Text as A

import Options.Applicative (Parser, ParserInfo, argument, auto, footer, fullDesc, header, help, helper, info, long, metavar, option, short, str, switch, value)
import Options.Applicative.Util (AttoParser, anyOf, showSepChars, splitWith)


-- | Options the user may give to the tool via the command line.
data Options = Options
  { word       :: !String     -- ^ Positional argument: word to look up
  , maxShown   :: !Int        -- ^ Max hits to show
  , sections   :: ![Section]  -- ^ What to show
  , onlyUsage  :: !Bool       -- ^ Only show the usage field
  , onlyLookup :: !Bool       -- ^ Look up this word directly
  , wrap       :: !Int        -- ^ When to wrap the text
  }

-- | Create an info type from our options, adding help text and other nice
-- features.
options :: ParserInfo Options
options = info
  (helper <*> pOptions)
  (  header "Query `duden.de' from the comfort of your command line."
  <> footer ("For options that take more than one argument, either wrap the \
             \argument in quotes or separate with one of the following \
             \characters (note that spaces between arguments are not \
             \allowed):" ++ showSepChars sepChars)
  <> fullDesc
  )

-- | Options we have and how to parse them.
pOptions :: Parser Options
pOptions =
  Options <$> pWord
          <*> pMaxShown
          <*> pSections
          <*> pOnlyUsage
          <*> pLookup
          <*> pWrap

-- | Word to look up.
pWord :: Parser String
pWord = argument str (metavar "STR")

-- | Max hits to show.
pMaxShown :: Parser Int
pMaxShown = option auto
   ( short 'm'
  <> long "max-shown"
  <> metavar "N"
  <> help "Show only the first N hits.  N has to be a natural number; a \
          \value of 0 shows all hits.  Default: 2"
  <> value 2
   )

-- | Sections to be displayed, making sure that no sections appear twice.
pSections :: Parser [Section]
pSections = nub <$> option (pSection `splitWith` sepChars)
   ( long "sections"
  <> short 's'
  <> metavar "S"
  <> help "Sections to print (in order) in the final output.  \
          \Default: show everything (WordClass,Usage,Meaning,Synonyms)."
  <> value [WordClass, Usage, Meaning, Synonyms]
   )
 where
  -- | Parse user input into a proper 'MealTime'.
  pSection :: AttoParser Section
  pSection = anyOf [ (WordClass, ["w", "c", "k"])
                   , (Usage    , ["u", "ben"])
                   , (Meaning  , ["m", "bed"])
                   , (Synonyms , ["s"])
                   ]
          <* A.skipWhile (`notElem` sepChars)

-- | Only show the 'Bedeutung' section.
pOnlyUsage :: Parser Bool
pOnlyUsage = switch
   ( long "usage"
  <> short 'u'
  <> help "Only show the 'Bedeutung' section."
   )

-- | Directly look up the word.
pLookup :: Parser Bool
pLookup = switch
   ( long "lookup"
  <> short 'l'
  <> help "Directly look up the word (instead of searching for it and \
          \looking up the entries of the search results)."
   )

-- | When to wrap the text in columns.
pWrap :: Parser Int
pWrap = option auto
   ( short 'w'
  <> long "wrap"
  <> metavar "N"
  <> help "Wrap text at N columns.  N has to be a natural number; a \
          \value of 0 indicates no line wrapping.  Default: 0"
  <> value 0
   )

-- | Our separator characters.
sepChars :: [Char]
sepChars = [',', ';', ':', '.']
