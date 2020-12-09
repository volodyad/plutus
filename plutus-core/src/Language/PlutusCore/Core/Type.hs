{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Language.PlutusCore.Core.Type
    ( Kind(..)
    , Type(..)
    , Term(..)
    , Version(..)
    , Program(..)
    , UniOf
    , Normalized(..)
    , ToAnnotation(..)
    , HasUniques
    , defaultVersion
    -- * Helper functions
    , toTerm
    , mapFun
    )
where

import           PlutusPrelude

import           Language.PlutusCore.Name
import           Language.PlutusCore.Universe

import           Control.Lens
import           Data.Hashable
import           GHC.Exts                     (Constraint)
import           Instances.TH.Lift            ()

{- Note [Annotations and equality]
Equality of two things does not depend on their annotations.
So don't use @deriving Eq@ for things with annotations.
-}

data Kind ann
    = Type ann
    | KindArrow ann (Kind ann) (Kind ann)
    deriving (Show, Functor, Generic, NFData, Lift, Hashable)

-- | A 'Type' assigned to expressions.
data Type tyname uni ann
    = TyVar ann tyname
    | TyFun ann (Type tyname uni ann) (Type tyname uni ann)
    | TyIFix ann (Type tyname uni ann) (Type tyname uni ann)
      -- ^ Fix-point type, for constructing self-recursive types
    | TyForall ann tyname (Kind ann) (Type tyname uni ann)
    | TyBuiltin ann (Some (TypeIn uni)) -- ^ Builtin type
    | TyLam ann tyname (Kind ann) (Type tyname uni ann)
    | TyApp ann (Type tyname uni ann) (Type tyname uni ann)
    deriving (Show, Functor, Generic, NFData, Hashable)

data Term tyname name uni fun ann
    = Var ann name -- ^ a named variable
    | TyAbs ann tyname (Kind ann) (Term tyname name uni fun ann)
    | LamAbs ann name (Type tyname uni ann) (Term tyname name uni fun ann)
    | Apply ann (Term tyname name uni fun ann) (Term tyname name uni fun ann)
    | Constant ann (Some (ValueOf uni)) -- ^ a constant term
    | Builtin ann fun
    | TyInst ann (Term tyname name uni fun ann) (Type tyname uni ann)
    | Unwrap ann (Term tyname name uni fun ann)
    | IWrap ann (Type tyname uni ann) (Type tyname uni ann) (Term tyname name uni fun ann)
    | Error ann (Type tyname uni ann)
    deriving (Show, Functor, Generic, NFData, Hashable)

-- | Version of Plutus Core to be used for the program.
data Version ann
    = Version ann Natural Natural Natural
    deriving (Show, Functor, Generic, NFData, Hashable)

-- | A 'Program' is simply a 'Term' coupled with a 'Version' of the core language.
data Program tyname name uni fun ann = Program ann (Version ann) (Term tyname name uni fun ann)
    deriving (Show, Functor, Generic, NFData, Hashable)

-- | Extract the universe from a type.
type family UniOf a :: * -> *

type instance UniOf (Term tyname name uni fun ann) = uni

newtype Normalized a = Normalized
    { unNormalized :: a
    } deriving (Show, Eq, Functor, Foldable, Traversable, Generic)
      deriving newtype (NFData, Pretty, PrettyBy config)
      deriving Applicative via Identity

-- | All kinds of uniques an entity contains.
type family HasUniques a :: Constraint
type instance HasUniques (Kind ann) = ()
type instance HasUniques (Type tyname uni ann) = HasUnique tyname TypeUnique
type instance HasUniques (Term tyname name uni fun ann) =
    (HasUnique tyname TypeUnique, HasUnique name TermUnique)
type instance HasUniques (Program tyname name uni fun ann) =
    HasUniques (Term tyname name uni fun ann)

-- | The default version of Plutus Core supported by this library.
defaultVersion :: ann -> Version ann
defaultVersion ann = Version ann 1 0 0

toTerm :: Program tyname name uni fun ann -> Term tyname name uni fun ann
toTerm (Program _ _ term) = term

class ToAnnotation term where
    type Annotation term :: *
    toAnnotation :: term -> Annotation term

instance ToAnnotation (Term tyname name uni fun ann) where
    type Annotation (Term tyname name uni fun ann) = ann
    toAnnotation (Var ann _       ) = ann
    toAnnotation (TyAbs ann _ _ _ ) = ann
    toAnnotation (Apply ann _ _   ) = ann
    toAnnotation (Constant ann _  ) = ann
    toAnnotation (Builtin  ann _  ) = ann
    toAnnotation (TyInst ann _ _  ) = ann
    toAnnotation (Unwrap ann _    ) = ann
    toAnnotation (IWrap ann _ _ _ ) = ann
    toAnnotation (Error ann _     ) = ann
    toAnnotation (LamAbs ann _ _ _) = ann

instance ToAnnotation (Type tyname uni ann) where
    type Annotation (Type tyname uni ann) = ann
    toAnnotation (TyVar ann _       ) = ann
    toAnnotation (TyFun ann _ _     ) = ann
    toAnnotation (TyIFix ann _ _    ) = ann
    toAnnotation (TyForall ann _ _ _) = ann
    toAnnotation (TyBuiltin ann _   ) = ann
    toAnnotation (TyLam ann _ _ _   ) = ann
    toAnnotation (TyApp ann _ _     ) = ann

instance ToAnnotation (Kind ann) where
    type Annotation (Kind ann) = ann
    toAnnotation (Type ann)          = ann
    toAnnotation (KindArrow ann _ _) = ann

-- | Map a function over the set of built-in functions.
mapFun :: (fun -> fun') -> Term tyname name uni fun ann -> Term tyname name uni fun' ann
mapFun f = go where
    go (LamAbs ann name ty body)  = LamAbs ann name ty (go body)
    go (TyAbs ann name kind body) = TyAbs ann name kind (go body)
    go (IWrap ann pat arg term)   = IWrap ann pat arg (go term)
    go (Apply ann fun arg)        = Apply ann (go fun) (go arg)
    go (Unwrap ann term)          = Unwrap ann (go term)
    go (Error ann ty)             = Error ann ty
    go (TyInst ann term ty)       = TyInst ann (go term) ty
    go (Var ann name)             = Var ann name
    go (Constant ann con)         = Constant ann con
    go (Builtin ann fun)          = Builtin ann (f fun)
