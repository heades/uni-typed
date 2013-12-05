{-# LANGUAGE NoMonomorphismRestriction, PackageImports #-}
module Parser where

import Prelude
import Data.List
import Data.Char
import Control.Applicative hiding ((<|>),many,optional)
import "mtl" Control.Monad.Identity hiding (fix)
import Control.Monad.IO.Class
import Control.Monad.State

import Unbound.LocallyNameless hiding (name,Infix,Val,Con,Equal,Refl, rec)
import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language

import Syntax

lexer = haskellStyle {
          Token.reservedOpNames = ["fun"{-lambda-}, "->", "Mu", "<-"]
        }
tokenizer = Token.makeTokenParser lexer

ident      = Token.identifier tokenizer
reserved   = Token.reserved tokenizer
reservedOp = Token.reservedOp tokenizer
parens     = Token.parens tokenizer
angles     = Token.angles tokenizer
brackets   = Token.brackets tokenizer
braces     = Token.braces tokenizer
ws         = Token.whiteSpace tokenizer
natural    = Token.natural tokenizer
dot        = Token.dot tokenizer
comma      = Token.comma tokenizer
colon      = Token.colon tokenizer

-- TODO: Get global state working.
exprParser = ws >> expr

aterm = (parens expr) <|> var
expr = try lam <|> defn <|> app_parse 

defn = do
  reservedOp "def"
  name <- varName 
  reservedOp "<-"
  term <- expr
  return (Mu . bind name $ term)

atom a c = do
  reserved a
  return c

-- Parsers to be used by the expression parser.
lam_parse = do 
  reservedOp "fun"
  name <- varName
  reservedOp "->"
  body <- expr
  return . Lam . bind name $ body

app_parse = do
  ts <- many aterm
  return . foldl1 App $ ts
       
unexpColon msg = unexpected (" : "++msg)

varName' f msg = do
  n <- ident
  when (f (head n)) $ unexpColon msg
  return . s2n $ n
varName = varName' isUpper "Term variables must begin with a lowercase letter."

var' p c = do 
  var_name <- p
  return (c var_name)  
var   = var' varName Var

lam    = lam_parse

parseTerm :: String -> Term
parseTerm str = 
    case parse expr "" str of
      Left e  -> error $ show e
      Right r -> r

parseDef :: String -> Term
parseDef str = 
    case parse defn "" str of
      Left e  -> error $ show e
      Right r -> r
