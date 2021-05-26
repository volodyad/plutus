-- This ensures that we don't put *anything* about these functions into the interface
-- file, otherwise GHC can be clever about the ones that are always error, even though
-- they're NOINLINE!
{-# OPTIONS_GHC -O0 #-}
module PlutusTx.String.Internal
  ( String
  , appendString
  , emptyString
  , charToString
  , equalsString
  , encodeUtf8
  ) where

import           PlutusTx.ByteString.Internal as BS
import           PlutusTx.Utils               (mustBeReplaced)
import qualified Prelude                      as Haskell

-- | An opaque type representing Plutus Core strings.
data String

{-# NOINLINE appendString #-}
-- | Append two 'String's.
appendString :: String -> String -> String
appendString = mustBeReplaced "appendString"

{-# NOINLINE emptyString #-}
-- | An empty 'String'.
emptyString :: String
emptyString = mustBeReplaced "emptyString"

{-# NOINLINE charToString #-}
-- | Turn a 'Char' into a 'String'.
charToString :: Haskell.Char -> String
charToString = mustBeReplaced "charToString"

{-# NOINLINE equalsString #-}
-- | Check if two strings are equal
equalsString :: String -> String -> Haskell.Bool
equalsString = mustBeReplaced "equalsString"

{-# NOINLINE encodeUtf8 #-}
-- | Convert a String into a ByteString.
encodeUtf8 :: String -> BS.ByteString
encodeUtf8 = mustBeReplaced "encodeUtf8"
