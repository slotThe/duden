{- |
   Module      : Main
   Description : Entry point for the program
   Copyright   : (c) slotThe  2020 2021
   License     : AGPL
   Maintainer  : slotThe <soliditsallgood@mailbox.org>
   Stability   : experimental
   Portability : non-portable
-}
module Main
  ( main  -- :: IO ()
  ) where

import CLI.Parser (Options (Options, maxShown, onlyLookup, onlyUsage, sections, word, wrap), options)
import HTML.Parser (lookupWord, searchForWord)
import HTML.Types (Section (Usage))

import Control.Concurrent.Async (mapConcurrently)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Options.Applicative (execParser)


main :: IO ()
main = do
  Options{ maxShown, word, sections, onlyUsage, onlyLookup, wrap } <- execParser options
  man <- newManager tlsManagerSettings

  -- Sections to print.
  let sns = if onlyUsage then [Usage] else sections

  -- Whether to search for the word or look it up directly.
  if   onlyLookup
  then putTextLn =<< lookupWord man sns wrap word
  else do
    ws <- searchForWord man word
      >>= (if maxShown == 0 then id else take (fi maxShown))
       .> mapConcurrently (lookupWord man sns wrap)

    -- Output words in the order they appear on the website.
    traverse_ putTextLn ws
