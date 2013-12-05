{-# LANGUAGE NoMonomorphismRestriction, PackageImports #-}
module PrettyPrint (pPrint) where

import Prelude
import Data.List
import Data.Char
import Control.Monad.IO.Class
import Control.Applicative hiding (many)

import Unbound.LocallyNameless hiding (rec, apply)

import Syntax

pPrint :: Fresh m => Term -> m String

pPrint (Var n) = return $ name2String n

pPrint (Lam b) = do
  (x, t) <- unbind b
  strT <- pPrint t
  let strX = name2String x in
    return ("(fun "++strX++" <- "++strT++")")

pPrint (Mu b) = do
  (x, c) <- unbind b
  strC <- pPrint c
  let strX = name2String x in
    return ("def "++strX++" <- "++strC)

pPrint (App t1 t2) = do
  str1 <- pPrint t1
  str2 <- pPrint t2
  return ("("++str1++" "++str2++")")
