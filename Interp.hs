{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell, 
             DeriveDataTypeable,        FlexibleInstances, 
             MultiParamTypeClasses,     FlexibleContexts, 
                                        UndecidableInstances #-}

module Interp where 

import Prelude
import Data.List
import Unbound.LocallyNameless
import Unbound.LocallyNameless.Alpha
import Data.Typeable
import Control.Monad.IO.Class

import Syntax

eval :: MonadIO m => Fresh m => Term -> Int -> m Term

eval t 0 = return t

eval (Var x) _ = return $ Var x

eval (Lam binding) n = 
  let n' = n - 1 in 
    do (x, e) <- unbind binding
       e' <- eval e n'
       return (Lam (bind x e'))

eval (Mu binding) n = 
    do (f, e) <- unbind binding
       let s = subst f (Mu binding) e in
         eval s (n-1)

eval (App t1 t2) n = 
  let n' = n - 1 in 
  do e1 <- eval t1 n'
     e2 <- eval t2 n'
     case e1 of
       Lam binding -> 
        do (x, e1') <- (unbind binding)
           let s = subst x e2 e1' in
             let n'' = n - 1 in 
               eval s n''
       _ -> return $ App e1 e2

loopLam :: Term
loopLam = let n = s2n "x" in Lam (bind n (App (Var n) (Var n)))
loop = App loopLam loopLam

loopabit = do
    t <- runFreshMT (eval loop 10000)
    liftIO $ putStrLn $ show t

test = let a = s2n "A" in 
         let x = s2n "x" in 
           Mu (bind a (Lam (bind x (App (Var a) (App (Var a) (Var x))))))
