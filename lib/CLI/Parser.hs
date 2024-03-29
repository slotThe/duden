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

import Options.Applicative (Parser, ParserInfo, argument, auto, footer, fullDesc, header, help, helper, info, long, metavar, option, short, str, switch, value, ReadM)
import Options.Applicative.CmdLine.Util (AttoParser, anyOf, attoReadM, optionA, showSepChars, splitWith)
import Data.Attoparsec.Text ((<?>))


-- | Options the user may give to the tool via the command line.
data Options = Options
  { word       :: !String     -- ^ Positional argument: word to look up
  , maxShown   :: !Natural    -- ^ Max hits to show
  , sections   :: ![Section]  -- ^ What to show
  , onlyUsage  :: !Bool       -- ^ Only show the usage field
  , onlyLookup :: !Bool       -- ^ Look up this word directly
  , wrap       :: !Natural    -- ^ When to wrap the text
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
pWord = unwords <$> some (argument str (metavar "STR"))

-- | Max hits to show.
pMaxShown :: Parser Natural
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
pSections = nub <$> optionA (pSection `splitWith` sepChars)
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
pWrap :: Parser Natural
pWrap = option pWrap'
   ( short 'w'
  <> long "wrap"
  <> metavar "N"
  <> help "Wrap text at N columns.  N has to be a natural number \
          \above 29 or 0.  A value of 0 indicates no line wrapping.  \
          \Default: 0"
  <> value 0
   )
 where
  pWrap' :: ReadM Natural =
    (A.decimal >>= \n -> if n == 0 || n >= 30 then pure n else empty
      <?> "pWrap: Argument should be at least 30 (or 0)")
    & attoReadM

-- | Our separator characters.
sepChars :: [Char]
sepChars = [',', ';', ':', '.']
