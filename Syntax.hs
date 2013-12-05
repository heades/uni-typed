{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell, 
             DeriveDataTypeable,        FlexibleInstances, 
             MultiParamTypeClasses,     FlexibleContexts, 
                                        UndecidableInstances #-}
module Syntax where

import Prelude
import Data.List
import Unbound.LocallyNameless
import Unbound.LocallyNameless.Alpha
import Data.Typeable
import Control.Monad.IO.Class

type TmName = Name Term              -- Term names
data Term =
    Var TmName                       -- Term variable
  | Lam (Bind TmName Term)           -- Lambda abstraction
  | Mu (Bind TmName Term)            -- Local definition
  | App Term Term                    -- Term application
  deriving (Show, Typeable)

$(derive [''Term])
instance Alpha Term

instance Subst Term Term where
  isvar (Var x) = Just (SubstName x)
  isvar _ = Nothing

