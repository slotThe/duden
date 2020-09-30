{- |
   Module      : Prelude
   Description : Small custom prelude for commonly used things
   Copyright   : (c) slotThe, 2020
   License     : AGPL
   Maintainer  : slotThe <soliditsallgood@mailbox.org>
   Stability   : experimental
   Portability : non-portable
-}
module Prelude
  ( module Exports
  , BS.ByteString
  , LazyByteString
  , Text
  , decodeUtf8
  , nonEmpty
  , putTextLn   -- :: Text -> IO ()
  , tshow       -- :: Show a => a -> Text
  , (.>)        -- :: (a -> b) -> (b -> c) -> (a -> c)
  , (%~~)       -- :: Functor f => Lens s t (f a) (f b) -> (a -> b) -> s -> t
  ) where

import BasePrelude as Exports hiding (option)
import Lens.Micro as Exports hiding (lazy)

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T
import qualified Data.Text.IO         as T

import Data.List.NonEmpty (nonEmpty)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)


type LazyByteString = BL.ByteString

putTextLn :: Text -> IO ()
putTextLn = T.putStrLn

tshow :: Show a => a -> Text
tshow = show .> T.pack

infixr 9 .>
-- | Covariant version of '(.)'.
(.>) :: (a -> b) -> (b -> c) -> (a -> c)
(.>) = (>>>)

infixr 4 %~~
-- | Apply a function through a functor.
(%~~) :: Functor f => Lens s t (f a) (f b) -> (a -> b) -> s -> t
(%~~) l ab s = s & l.mapped %~ ab
