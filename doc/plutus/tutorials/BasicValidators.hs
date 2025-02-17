{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
module BasicValidators where

import qualified PlutusCore.Default   as PLC
import           PlutusTx
import           PlutusTx.Lift
import           PlutusTx.Prelude

import           Ledger               hiding (validatorHash)
import           Ledger.Ada
import           Ledger.Typed.Scripts
import           Ledger.Value

import qualified Data.ByteString.Lazy as BSL

import           Codec.Serialise

import           Prelude              (IO, print, show)
import qualified Prelude              as Haskell


myKeyHash :: PubKeyHash
myKeyHash = Haskell.undefined

-- BLOCK1
-- | A specific date.
newtype Date = Date Integer
-- | Either a specific end date, or "never".
data EndDate = Fixed Integer | Never

-- 'unstableMakeIsData' is a TemplateHaskell function that takes a type name and
-- generates an 'IsData' instance definition for it. It should work for most
-- types, including newtypes and sum types. For production usage use 'makeIsDataIndexed'
-- which ensures that the output is stable across time.
unstableMakeIsData ''Date
unstableMakeIsData ''EndDate

-- BLOCK2
alwaysSucceeds :: Data -> Data -> Data -> ()
alwaysSucceeds _ _ _ = ()

alwaysFails :: Data -> Data -> Data -> ()
alwaysFails _ _ _ = error ()

-- We can use 'compile' to turn a validator function into a compiled Plutus Core program.
-- Here's a reminder of how to do it.
alwaysSucceedsCompiled :: CompiledCode (Data -> Data -> Data -> ())
alwaysSucceedsCompiled = $$(compile [|| alwaysSucceeds ||])
-- BLOCK3
-- | Checks if a date is before the given end date.
beforeEnd :: Date -> EndDate -> Bool
beforeEnd (Date d) (Fixed e) = d <= e
beforeEnd (Date _) Never     = True

-- | Check that the date in the redeemer is before the limit in the datum.
validateDate :: Data -> Data -> Data -> ()
-- The 'check' function takes a 'Bool' and fails if it is false.
-- This is handy since it's more natural to talk about booleans.
validateDate datum redeemer _ = check $ case (fromData datum, fromData redeemer) of
    -- We can decode both the arguments at the same time: 'Just' means that
    -- decoding succeeded.
    (Just endDate, Just date) -> beforeEnd date endDate
    -- One or the other failed to decode.
    _                         -> False
-- BLOCK4
validatePayment :: Data -> Data -> Data -> ()
validatePayment _ _ ctx = check $ case fromData ctx of
    Just valCtx ->
        -- The 'TxInfo' in the validation context is the representation of the
        -- transaction being validated
        let txinfo = scriptContextTxInfo valCtx
        -- 'pubKeyOutputsAt' collects the 'Value' at all outputs which pay to
        -- the given public key hash
            values = pubKeyOutputsAt myKeyHash txinfo
        -- 'fold' sums up all the values, we assert that there must be more
        -- than 1 Ada (more stuff is fine!)
        in fold values `geq` adaValueOf 1
    _ -> False
-- BLOCK5
data DateValidator
instance ValidatorTypes DateValidator where
    type instance RedeemerType DateValidator = Date
    type instance DatumType DateValidator = EndDate
-- BLOCK6
validateDateTyped :: EndDate -> Date -> ScriptContext -> Bool
validateDateTyped endDate date _ = beforeEnd date endDate

validateDateWrapped :: Data -> Data -> Data -> ()
validateDateWrapped = wrapValidator validateDateTyped
-- BLOCK7
dateInstance :: TypedValidator DateValidator
dateInstance = mkTypedValidator @DateValidator
    -- The first argument is the compiled validator.
    $$(compile [|| validateDateTyped ||])
    -- The second argument is a compiled wrapper.
    -- Unfortunately we can't just inline wrapValidator here for technical reasons.
    $$(compile [|| wrap ||])
    where
        wrap = wrapValidator

dateValidatorHash :: ValidatorHash
dateValidatorHash = validatorHash dateInstance

dateValidator :: Validator
dateValidator = validatorScript dateInstance
-- BLOCK8
serializedDateValidator :: BSL.ByteString
serializedDateValidator = serialise dateValidator

main :: IO ()
main = print serializedDateValidator
-- BLOCK9
