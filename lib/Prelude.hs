{- |
   Module      : Prelude
   Description : Small custom prelude for commonly used things
   Copyright   : (c) slotThe  2020 2021
   License     : AGPL
   Maintainer  : slotThe <soliditsallgood@mailbox.org>
   Stability   : experimental
   Portability : non-portable
-}
module Prelude
  ( module Exports

    -- * Types
  , BS.ByteString
  , LazyByteString
  , Text

    -- * Working with 'Text'
  , decodeUtf8  -- :: ByteString -> Text
  , putTextLn   -- :: Text -> IO ()
  , tshow       -- :: Show a => a -> Text
  , unlines     -- :: [Text] -> Text
  , writeFile   -- :: FilePath -> Text -> IO ()

    -- * Misc
  , nonEmpty    -- :: [a] -> Maybe (NonEmpty a)
  , (.>)        -- :: (a -> b) -> (b -> c) -> (a -> c)
  , fi          -- :: (Integral a, Num b) => a -> b
  ) where

import BasePrelude as Exports hiding (unlines, writeFile)

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T
import qualified Data.Text.IO         as T

import Data.List.NonEmpty (nonEmpty)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)


-- | Less confusion with a nice-to-read type alias.
type LazyByteString = BL.ByteString

putTextLn :: Text -> IO ()
putTextLn = T.putStrLn

writeFile :: FilePath -> Text -> IO ()
writeFile = T.writeFile

tshow :: Show a => a -> Text
tshow = show .> T.pack

unlines :: [Text] -> Text
unlines = T.unlines

infixr 9 .>
-- | Covariant version of '(.)'.
(.>) :: (a -> b) -> (b -> c) -> (a -> c)
(.>) = (>>>)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
