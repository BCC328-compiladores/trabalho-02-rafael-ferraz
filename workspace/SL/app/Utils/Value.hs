{-# LANGUAGE FlexibleContexts #-}

module Utils.Value where 

import Control.Monad.Except
import Prettyprinter
import Prettyprinter.Render.String

-- definition of values 

data Value 
  = VInt Int 
  | VBool Bool 
  | VFloat Float 
  | VString String
  deriving (Eq, Ord, Show)

(.+.) :: MonadError String m => Value -> Value -> m Value 
(VInt n1) .+. (VInt n2) = pure (VInt (n1 + n2))
v1 .+. v2 = throwError $ unlines ["Cannot sum:", renderString (layoutPretty defaultLayoutOptions (pretty v1)), "with", renderString (layoutPretty defaultLayoutOptions (pretty v2))]

(.-.) :: MonadError String m => Value -> Value -> m Value 
(VInt n1) .-. (VInt n2) = pure (VInt (n1 - n2))
v1 .-. v2 = throwError $ unlines ["Cannot subtract:", renderString (layoutPretty defaultLayoutOptions (pretty v1)), "with", renderString (layoutPretty defaultLayoutOptions (pretty v2))]

(.*.) :: MonadError String m => Value -> Value -> m Value 
(VInt n1) .*. (VInt n2) = pure (VInt (n1 * n2))
v1 .*. v2 = throwError $ unlines ["Cannot multiply:", renderString (layoutPretty defaultLayoutOptions (pretty v1)), "with", renderString (layoutPretty defaultLayoutOptions (pretty v2))]

(./.) :: MonadError String m => Value -> Value -> m Value 
(VInt n1) ./. (VInt n2) = pure (VInt (n1 `div` n2))
v1 ./. v2 = throwError $ unlines ["Cannot divide:", renderString (layoutPretty defaultLayoutOptions (pretty v1)), "with", renderString (layoutPretty defaultLayoutOptions (pretty v2))]

(.<.) :: MonadError String m => Value -> Value -> m Value 
(VInt n1) .<. (VInt n2) = pure (VBool (n1 < n2))
v1 .<. v2 = throwError $ unlines ["Cannot compare:", renderString (layoutPretty defaultLayoutOptions (pretty v1)), "with", renderString (layoutPretty defaultLayoutOptions (pretty v2))]

(.==.) :: MonadError String m => Value -> Value -> m Value 
(VInt n1) .==. (VInt n2) = pure (VBool (n1 == n2))
(VBool b1) .==. (VBool b2) = pure (VBool (b1 == b2))
(VFloat f1) .==. (VFloat f2) = pure (VBool (f1 == f2))
(VString s1) .==. (VString s2) = pure (VBool (s1 == s2))
v1 .==. v2 = throwError $ unlines ["Cannot compare:", renderString (layoutPretty defaultLayoutOptions (pretty v1)), "with", renderString (layoutPretty defaultLayoutOptions (pretty v2))]

(.||.) :: MonadError String m => Value -> Value -> m Value
(VBool b1) .||. (VBool b2) = pure (VBool (b1 || b2))
v1 .||. v2 = throwError $ unlines ["Cannot disjunct:", renderString (layoutPretty defaultLayoutOptions (pretty v1)), "with", renderString (layoutPretty defaultLayoutOptions (pretty v2))]

vnot :: MonadError String m => Value -> m Value 
vnot (VBool b) = pure (VBool (not b))
vnot e = throwError $ unlines ["Cannot negate:", renderString (layoutPretty defaultLayoutOptions (pretty e))]

instance Pretty Value where 
  pretty (VInt n) = pretty n 
  pretty (VBool b) = if b then (pretty "true") else (pretty "false")
  pretty (VFloat f) = pretty f
  pretty (VString s) = dquotes (pretty s)