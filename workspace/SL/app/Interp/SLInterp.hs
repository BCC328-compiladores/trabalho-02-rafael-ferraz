module Interp.SLInterp where

import Control.Monad.Except
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Prettyprinter
import Prettyprinter.Render.String

import Utils.Value 
import Frontend.Syntax.SLSyntax

-- main interpreter function

interp :: SL -> IO ()
interp p = do
    (r, _) <- runStateT (runExceptT (interpSL p)) Map.empty
    case r of
      Left err -> putStrLn err
      Right _ -> pure ()

-- interpreting programs

interpSL :: SL -> Interp ()
interpSL (SL blocks) = mapM_ interpBlock blocks

-- evaluating blocks

interpBlock :: Block -> Interp ()
interpBlock block
  = do
    case block of
      Func fname args ftype stmts -> interpFunc fname args ftype stmts
      Struct sname fields -> undefined "Struct interpretation not implemented"

-- evaluating functions

interpFunc :: Id -> [(Id, Type)] -> Type -> [Stmt] -> Interp ()
interpFunc fname args ftype stmts
  = do
    mapM_ interpStmt stmts

-- evaluating statements

interpStmt :: Stmt -> Interp ()
interpStmt (SAssign v e)
  = do
      val <- interpExp e
      updateVar v val
interpStmt (SRead v)
  = do
      str <- liftIO $ getLine
      let val = read str
      updateVar v (VInt val)
interpStmt (SPrint e)
  = do
      val <-interpExp e
      liftIO $ print val
interpStmt (SIf e blk1 blk2)
  = do 
      v <- interpExp e 
      case v of 
        VBool True-> mapM_ interpStmt blk1 
        VBool False -> mapM_ interpStmt blk2 
        _ -> throwError $ unlines ["Expecting a boolean value, but found:", renderString (layoutPretty defaultLayoutOptions (pretty e))]
interpStmt (SWhile e blk1)
  = do 
      v <- interpExp e 
      case v of 
        VBool False -> pure ()
        VBool True -> do 
          mapM_ interpStmt blk1 
          interpStmt (SWhile e blk1)
        _ -> throwError $ unlines ["Expecting a boolean value, but found:", renderString (layoutPretty defaultLayoutOptions (pretty e))]

-- evaluating expressions

interpExp :: Exp -> Interp Value 
interpExp (EValue v) = pure v
interpExp (EVar v) = askVar v
interpExp (e1 :+: e2)
  = do 
       v1 <- interpExp e1
       v2 <- interpExp e2
       v1 .+. v2
interpExp (e1 :*: e2)
  = do 
       v1 <- interpExp e1
       v2 <- interpExp e2
       v1 .*. v2
interpExp (e1 :<: e2)
  = do 
       v1 <- interpExp e1
       v2 <- interpExp e2
       v1 .<. v2
interpExp (e1 :==: e2)
  = do 
       v1 <- interpExp e1
       v2 <- interpExp e2
       v1 .==. v2
interpExp (e1 :||: e2)
  = do 
       v1 <- interpExp e1
       v2 <- interpExp e2
       v1 .||. v2
interpExp (ENot e)
  = do 
      v <- interpExp e 
      vnot v 


-- monad and environment definition

type Env = Map String Value
type Interp a = ExceptT String (StateT Env IO) a

askVar :: String -> Interp Value
askVar s
  = do
      env <- get
      case Map.lookup s env of
        Nothing -> throwError $ unwords ["Undefined variable:", s]
        Just val -> pure val

updateVar :: String -> Value -> Interp ()
updateVar s val = modify (Map.insert s val)