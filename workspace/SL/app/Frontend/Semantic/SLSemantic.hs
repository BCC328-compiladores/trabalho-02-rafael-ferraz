module Frontend.Semantic.SLSemantic where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.Except
import Data.Bifunctor (bimap)

import Utils.Value
import Frontend.Syntax.SLSyntax

type Env = Map Id Type

data SemanticError
  = UnboundVariable Id String  -- Variable name and context
  | UnboundFunction Id String  -- Function name and context
  | TypeMismatch Type Type String  -- Expected, Actual and context
  deriving (Show)

type Checker a = ReaderT Env (Except SemanticError) a

semanticAnalysis :: SL -> Either String ()
semanticAnalysis (SL blocks) = do
  env <- return (initialEnv blocks)
  checker <- return $ mapM_ checkBlock blocks
  bimap show id $ runExcept (runReaderT checker env)

initialEnv :: [Block] -> Env
initialEnv blocks = Map.fromList $ concatMap getInitialEnv blocks

getInitialEnv :: Block -> [(Id, Type)]
getInitialEnv (Func fname args ftype _) = [(fname, FuncT fname args ftype)]
getInitialEnv (Struct sname fields) = [(sname, StructT sname fields)]

checkBlock :: Block -> Checker ()
checkBlock (Func _ args _ stmts) = do
  let argEnv = Map.fromList args
  let varsEnv = initializeVars stmts
  local (Map.union argEnv . Map.union varsEnv) $
    mapM_ checkStmt stmts    

checkBlock (Struct sname fields) = do
  let structType = StructT sname fields
  local (Map.insert sname structType) $ return ()

initializeVars :: [Stmt] -> Env
initializeVars stmts = Map.fromList $ concatMap getInitialVars stmts

getInitialVars :: Stmt -> [(Id, Type)]
getInitialVars (SDeclare var varType) = [(var, varType)]
getInitialVars (SInit var varType _) = [(var, varType)]
getInitialVars _ = []

checkStmt :: Stmt -> Checker ()
checkStmt (SAssign var expr) = do
  env <- ask
  case Map.lookup var env of
    Just t -> do
      expType <- checkExp expr
      if t == expType
        then return ()
        else throwError $ TypeMismatch t expType "in SAssign"
    Nothing -> throwError $ UnboundVariable var "in SAssign"

checkStmt (SAssignAt var index expr) = do
  env <- ask
  indexType <- checkIndex index
  if indexType == Int
    then case Map.lookup var env of
          Just t -> do
            expType <- checkExp expr
            if sameType t expType
              then return ()
              else throwError $ TypeMismatch t expType "in SAssignAt"
          Nothing -> throwError $ UnboundVariable var "in SAssignAt"
    else throwError $ TypeMismatch Int indexType "in SAssignAt"

checkStmt (SInit var varType expr) = do
  expType <- checkExp expr
  if sameType varType expType
    then local (Map.insert var varType) $ return ()
    else throwError $ TypeMismatch varType expType "in SInit"

checkStmt (SDeclare _ _) = return ()

checkStmt (SRead _) = return ()

checkStmt (SPrint expr) = do
  _ <- checkExp expr
  return ()

checkStmt (SIf expr thenStmts elseStmts) = do
  expType <- checkExp expr
  if expType == Bool
    then do
      env <- ask
      let thenEnv = Map.union env (initializeVars thenStmts)
      local (const thenEnv) $ mapM_ checkStmt thenStmts
      let elseEnv = Map.union env (initializeVars elseStmts)
      local (const elseEnv) $ mapM_ checkStmt elseStmts
    else throwError $ TypeMismatch Bool expType "in SIf"

checkStmt (SWhile expr whileStmts) = do
  expType <- checkExp expr
  if expType == Bool
    then do
      env <- ask
      let whileEnv = Map.union env (initializeVars whileStmts)
      local (const whileEnv) $ mapM_ checkStmt whileStmts
    else throwError $ TypeMismatch Bool expType "in SWhile"

checkStmt (SReturn expr) = do
  _ <- checkExp expr
  return ()

checkExp :: Exp -> Checker Type
checkExp (EValue value) = checkValue value

checkExp (EVar name) = do
  env <- ask
  case Map.lookup name env of
    Just t -> return t
    Nothing -> throwError $ UnboundVariable name "in EVar"

checkExp (ENot expr) = do
  t <- checkExp expr
  if t == Bool
    then return Bool
    else throwError $ TypeMismatch Bool t "in ENot"

checkExp (EArray values) = do
  valueTypes <- mapM checkValue values
  case valueTypes of
    [] -> return (IntArray IEmpty) -- Default to array of int if empty
    (t:ts) -> if all (== t) ts
                then case t of
                       Int -> return $ IntArray IEmpty -- Placeholder index
                       Bool -> return $ BoolArray IEmpty
                       Float -> return $ FloatArray IEmpty
                       StructT name _ -> return $ StructTArray name IEmpty
                       _ -> throwError $ TypeMismatch (IntArray IEmpty) t "in EArray"
                else throwError $ TypeMismatch t (head (filter (/= t) ts)) "in EArray"

checkExp (EArrayAt arrayId index) = do
  arrayType <- checkExp (EVar arrayId)
  indexType <- checkIndex index
  if indexType == Int
    then case arrayType of
      IntArray i -> return $ IntArray i
      BoolArray i -> return $ BoolArray i
      FloatArray i -> return $ FloatArray i
      StructTArray structName i -> return $ StructTArray structName i
      _ -> throwError $ TypeMismatch (IntArray IEmpty) arrayType "in EArrayAt"
    else throwError $ TypeMismatch Int indexType "in EArrayAt"

checkExp (EArrayAtField arrayId index fieldId) = do
  env <- ask
  arrayType <- checkExp (EVar arrayId)
  fieldType <- checkExp (EVarField arrayId fieldId)
  indexType <- checkIndex index
  case arrayType of
    StructTArray structName idx -> do
      idxType <- checkIndex idx
      if indexType == idxType
        then case Map.lookup structName env of
          Just (StructT _ fields) -> case lookup fieldId fields of
                                      Just fType -> if fType == fieldType
                                                      then return fieldType
                                                      else throwError $ TypeMismatch fType fieldType "in EArrayAtField"
                                      Nothing -> throwError $ UnboundVariable (arrayId ++ "." ++ fieldId) "in EArrayAtField"
          Just _ -> throwError $ TypeMismatch (StructT structName []) arrayType "in EArrayAtField"
          Nothing -> throwError $ UnboundVariable structName "in EArrayAtField"
        else throwError $ TypeMismatch Int indexType "in EArrayAtField"
    _ -> throwError $ TypeMismatch (StructTArray "" IEmpty) arrayType "in EArrayAtField"

checkExp (EVarField var field) = do
  varType <- checkExp (EVar var)
  case varType of
    StructT _ fields -> do
      case lookup field fields of
        Just fieldType -> return fieldType
        Nothing -> throwError $ UnboundVariable (var ++ "." ++ field) "in EVarField"
    StructTArray sname index -> do
      indexType <- checkIndex index
      if indexType == Int
        then do
          env <- ask
          case Map.lookup sname env of
            Just (StructT _ fields) -> do
              case lookup field fields of
                Just fieldType -> return fieldType
                Nothing -> throwError $ UnboundVariable (var ++ "." ++ field) "in EVarField"
            Just _ -> throwError $ TypeMismatch (StructT sname []) varType "in EVarField"
            Nothing -> throwError $ UnboundVariable sname "in EVarField"
        else throwError $ TypeMismatch Int indexType "in EVarField"
    _ -> throwError $ TypeMismatch (StructT var []) varType "in EVarField"

checkExp (ECall fname args) =
  do
    funcType <- checkExp (EVar fname)
    argTypes <- mapM checkExp args
    case funcType of
      FuncT _ param retType -> do
        let paramTypes = map snd param
        if length paramTypes == length argTypes && all (uncurry sameType) (zip paramTypes argTypes)
          then return retType
          else throwError $ TypeMismatch (FuncT fname (zip (map fst param) argTypes) retType) funcType "in ECall"
      _ -> throwError $ UnboundFunction fname "in ECall"

checkExp (EStruct structName args) = do
  env <- ask
  case Map.lookup structName env of
    Just (StructT _ fields) -> do
      argTypes <- mapM checkExp args
      let fieldTypes = map snd fields
      if argTypes == fieldTypes
        then return $ StructT structName fields
        else throwError $ TypeMismatch (StructT structName fields) (StructT structName (zip (map fst fields) argTypes)) "in EStruct"
    Just errType -> throwError $ TypeMismatch (StructT structName []) errType "in EStruct"
    Nothing -> throwError $ UnboundVariable structName "in EStruct"

checkExp (e1 :<=: e2) = checkRelational e1 e2
checkExp (e1 :<: e2) = checkRelational e1 e2
checkExp (e1 :>=: e2) = checkRelational e1 e2
checkExp (e1 :>: e2) = checkRelational e1 e2
checkExp (e1 :==: e2) = checkRelational e1 e2
checkExp (e1 :!=: e2) = checkRelational e1 e2
checkExp (e1 :||: e2) = checkLogical e1 e2
checkExp (e1 :&&: e2) = checkLogical e1 e2
checkExp (e1 :+: e2) = checkArithmetical e1 e2
checkExp (e1 :-: e2) = checkArithmetical e1 e2
checkExp (e1 :*: e2) = checkArithmetical e1 e2
checkExp (e1 :/: e2) = checkArithmetical e1 e2

checkRelational :: Exp -> Exp -> Checker Type
checkRelational e1 e2 = do
  t1 <- checkExp e1
  t2 <- checkExp e2
  if t1 == t2 && (t1 == Int || t1 == Float || t1 == Bool)
    then return Bool
    else throwError $ TypeMismatch (if t1 /= t2 then t1 else t2) (if t1 /= t2 then t2 else t1) "in Relational Expression"

checkLogical :: Exp -> Exp -> Checker Type
checkLogical e1 e2 = do
  t1 <- checkExp e1
  t2 <- checkExp e2
  if t1 == t2 && (t1 == Bool)
    then return Bool
    else throwError $ TypeMismatch (if t1 /= t2 then t1 else t2) (if t1 /= t2 then t2 else t1) "in Logical Expression"

checkArithmetical :: Exp -> Exp -> Checker Type
checkArithmetical e1 e2 = do
  t1 <- checkExp e1
  t2 <- checkExp e2
  if t1 == t2 && (t1 == Int || t1 == Float)
    then return t1
    else throwError $ TypeMismatch (if t1 /= t2 then t1 else t2) (if t1 /= t2 then t2 else t1) "in Arithmetical Expression"

checkValue :: Value -> Checker Type
checkValue (VInt _) = return Int
checkValue (VBool _) = return Bool
checkValue (VFloat _) = return Float
checkValue (VString _) = return String

checkIndex :: Index -> Checker Type
checkIndex (IValue value) = checkValue value

checkIndex (IVar name) = do
  env <- ask
  case Map.lookup name env of
    Just t -> return t
    Nothing -> throwError $ UnboundVariable name "in IVar"

checkIndex (IAdd i1 i2) = do
  t1 <- checkIndex i1
  t2 <- checkIndex i2
  if t1 == Int && t2 == Int
    then return Int
    else throwError $ TypeMismatch Int (if t1 /= Int then t1 else t2) "in IAdd"

checkIndex (ISub i1 i2) = do
  t1 <- checkIndex i1
  t2 <- checkIndex i2
  if t1 == Int && t2 == Int
    then return Int
    else throwError $ TypeMismatch Int (if t1 /= Int then t1 else t2) "in ISub"

checkIndex IEmpty = return Int