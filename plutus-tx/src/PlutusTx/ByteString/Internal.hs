-- This ensures that we don't put *anything* about these functions into the interface
-- file, otherwise GHC can be clever about the ones that are always error, even though
-- they're NOINLINE!
{-# OPTIONS_GHC -O0 #-}
module PlutusTx.ByteString.Internal
  ( ByteString
  , concatenate
  , takeByteString
  , dropByteString
  , emptyByteString
  , sha2_256
  , sha3_256
  , verifySignature
  , equalsByteString
  , lessThanByteString
  , greaterThanByteString
  , decodeUtf8
  ) where

import {-# SOURCE #-}           PlutusTx.String.Internal as String
import                          PlutusTx.Utils           (mustBeReplaced)

-- | An opaque type representing Plutus Core ByteStrings.
data ByteString

{-# NOINLINE concatenate #-}
-- | Concatenates two 'ByteString's.
concatenate :: ByteString -> ByteString -> ByteString
concatenate = mustBeReplaced "concatenate"

{-# NOINLINE takeByteString #-}
-- | Returns the n length prefix of a 'ByteString'.
takeByteString :: Integer -> ByteString -> ByteString
takeByteString = mustBeReplaced "takeByteString"

{-# NOINLINE dropByteString #-}
-- | Returns the suffix of a 'ByteString' after n elements.
dropByteString :: Integer -> ByteString -> ByteString
dropByteString = mustBeReplaced "dropByteString"

{-# NOINLINE emptyByteString #-}
-- | An empty 'ByteString'.
emptyByteString :: ByteString
emptyByteString = mustBeReplaced "emptyByteString"

{-# NOINLINE sha2_256 #-}
-- | The SHA2-256 hash of a 'ByteString'
sha2_256 :: ByteString -> ByteString
sha2_256 = mustBeReplaced "sha2_256"

{-# NOINLINE sha3_256 #-}
-- | The SHA3-256 hash of a 'ByteString'
sha3_256 :: ByteString -> ByteString
sha3_256 = mustBeReplaced "sha3_256"

{-# NOINLINE verifySignature #-}
-- | Verify that the signature is a signature of the message by the public key.
verifySignature :: ByteString -> ByteString -> ByteString -> Bool
verifySignature = mustBeReplaced "verifySignature"

{-# NOINLINE equalsByteString #-}
-- | Check if two 'ByteString's are equal.
equalsByteString :: ByteString -> ByteString -> Bool
equalsByteString = mustBeReplaced "equalsByteString"

{-# NOINLINE lessThanByteString #-}
-- | Check if one 'ByteString' is less than another.
lessThanByteString :: ByteString -> ByteString -> Bool
lessThanByteString = mustBeReplaced "lessThanByteString"

{-# NOINLINE greaterThanByteString #-}
-- | Check if one 'ByteString' is greater than another.
greaterThanByteString :: ByteString -> ByteString -> Bool
greaterThanByteString = mustBeReplaced "greaterThanByteString"

{-# NOINLINE decodeUtf8 #-}
-- | Converts a ByteString to a String.
decodeUtf8 :: ByteString -> String.String
decodeUtf8 = mustBeReplaced "decodeUtf8"
