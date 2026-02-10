module Frontend.Syntax.SLSyntax where

import Data.Tree
import Prettyprinter
import Prettyprinter.Render.String
import Utils.Value

data SL
  = SL [Block]
    deriving (Eq, Ord, Show)

type Id = String

data Block
  = Func Id [(Id, Type)] Type [Stmt]
  | Struct Id [(Id, Type)]
  deriving (Eq, Ord, Show)

data Stmt
  = SAssign Id Exp
  | SAssignAt Id Index Exp
  | SInit Id Type Exp
  | SDeclare Id Type
  | SRead Id
  | SPrint Exp
  | SIf Exp [Stmt] [Stmt]
  | SWhile Exp [Stmt]
  | SReturn Exp
  deriving (Eq, Ord, Show)

data Exp
  = EValue Value 
  | EVar Id
  | EVarField Id Id
  | ENot Exp
  | EArray [Value]
  | EArrayAt Id Index
  | EArrayAtField Id Index Id
  | ECall Id [Exp]
  | EStruct Id [Exp]
  | Exp :<=: Exp 
  | Exp :<: Exp
  | Exp :>=: Exp 
  | Exp :>: Exp 
  | Exp :==: Exp
  | Exp :!=: Exp
  | Exp :||: Exp 
  | Exp :&&: Exp 
  | Exp :+: Exp
  | Exp :-: Exp
  | Exp :*: Exp
  | Exp :/: Exp
  deriving (Eq, Ord, Show)

data Index
  = IValue Value
  | IVar Id
  | IAdd Index Index
  | ISub Index Index
  | IEmpty
  deriving (Eq, Ord, Show)

type Size = Int

data Type
  = Int
  | Void
  | Bool
  | Float
  | String
  | IntArray Index
  | BoolArray Index
  | FloatArray Index
  | StructT Id
  | StructTArray Id Index
  deriving (Eq, Ord, Show)

parserTree :: SL -> String
parserTree (SL blocks) = drawTree (Node "SL" (addBlocks blocks))

addBlocks :: [Block] -> [Tree String]
addBlocks [] = []
addBlocks (block:blocks) =
  let blockNode = case block of
        Func name args retType stmts -> Node ("Func " ++ name)
          [Node "Args" (addArgs args), Node "RetType" [Node (show retType) []], Node "Stmts" (addStmts stmts)]
        Struct name fields -> Node ("Struct " ++ name) [Node "Fields" (addFields fields)]
  in blockNode:(addBlocks blocks)

addArgs :: [(Id, Type)] -> [Tree String]
addArgs [] = []
addArgs ((variable, varType):args) = Node ("Arg " ++ variable ++ " : " ++ show varType) []:(addArgs args)

addFields :: [(Id, Type)] -> [Tree String]
addFields [] = []
addFields ((variable, varType):fields) = Node ("Field " ++ variable ++ " : " ++ show varType) []:(addFields fields)

addStmts :: [Stmt] -> [Tree String]
addStmts [] = []
addStmts (stmt:stmts) = 
  let stmtNode = case stmt of
        SDeclare variable varType -> Node ("SDeclare " ++ variable ++ " : " ++ show varType) []
        SInit variable varType expression -> Node ("SInit " ++ variable ++ " : " ++ show varType) [addExp expression]
        SAssign variable expression -> Node ("SAssign " ++ variable) [addExp expression]
        SAssignAt variable index expression -> 
          Node ("SAssignAt " ++ variable ++ "[Index]") [(Node "Index" [addIndex index]), addExp expression]
        SRead variable -> Node ("SRead " ++ variable) []
        SPrint expression -> Node "SPrint" [addExp expression]
        SIf cond thenStmts elseStmts ->
          Node "SIf" [addExp cond, Node "Then" (addStmts thenStmts), Node "Else" (addStmts elseStmts)]
        SWhile cond whileStmts ->
          Node "SWhile" [addExp cond, Node "Body" (addStmts whileStmts)]
        SReturn ret -> Node "SReturn" [addExp ret]
  in stmtNode:(addStmts stmts)

addExp :: Exp -> Tree String
addExp expression = case expression of
  EValue val -> Node ("EValue " ++ show val) []
  EVar name -> Node ("EVar " ++ name) []
  EVarField name field -> Node ("EVarField " ++ name ++ "." ++ field) []
  ENot e -> Node "ENot" [addExp e]
  EArray values -> Node ("EArray " ++ show values) []
  EArrayAt arrayId index -> Node ("EArrayAt " ++ arrayId ++ "[Index]") [(Node "Index" [addIndex index])]
  EArrayAtField arrayId index field -> Node ("EArrayAtField " ++ arrayId ++ "[Index]." ++ field) [(Node "Index" [addIndex index])]
  ECall func args -> Node ("ECall " ++ func) (map addExp args)
  EStruct structName args -> Node ("EStruct " ++ structName) (map addExp args)
  e1 :<=: e2 -> Node ":<=:" [addExp e1, addExp e2]
  e1 :<: e2  -> Node ":<:"  [addExp e1, addExp e2]
  e1 :>=: e2 -> Node ":>=:" [addExp e1, addExp e2]
  e1 :>: e2  -> Node ":>:"  [addExp e1, addExp e2]
  e1 :==: e2 -> Node ":==:" [addExp e1, addExp e2]
  e1 :!=: e2 -> Node ":!=:" [addExp e1, addExp e2]
  e1 :||: e2 -> Node ":||:" [addExp e1, addExp e2]
  e1 :&&: e2 -> Node ":&&:" [addExp e1, addExp e2]
  e1 :+: e2  -> Node ":+:"  [addExp e1, addExp e2]
  e1 :-: e2  -> Node ":-:"  [addExp e1, addExp e2]
  e1 :*: e2  -> Node ":*:"  [addExp e1, addExp e2]
  e1 :/: e2  -> Node ":/:"  [addExp e1, addExp e2]

addIndex :: Index -> Tree String
addIndex index = case index of
  IValue val -> Node ("IValue " ++ show val) []
  IVar name -> Node ("IVar " ++ name) []
  IAdd i1 i2 -> Node "IAdd" [addIndex i1, addIndex i2]
  ISub i1 i2 -> Node "ISub" [addIndex i1, addIndex i2]
  IEmpty -> Node "IEmpty" []

prettyPrint :: SL -> String
prettyPrint (SL blocks) = renderString (layoutPretty defaultLayoutOptions (vsep (map pretty blocks)))

instance Pretty SL where
  pretty (SL blocks) = vsep (map pretty blocks)

prettyArg :: (Id, Type) -> Doc ann
prettyArg (variable, varType) = pretty variable <+> pretty ":" <+> pretty varType

prettyField :: (Id, Type) -> Doc ann
prettyField (variable, varType) = pretty variable <+> pretty ":" <+> pretty varType <> pretty ";"

instance Pretty Block where
  pretty (Func name args retType stmts) =
    pretty "func" <+> pretty name <> pretty "(" <> hsep (punctuate comma (map prettyArg args)) <> pretty ")"
      <+> pretty ":" <+> pretty retType <+> pretty "{" <> line <> indent 2 (vsep (map pretty stmts)) <> line <> pretty "}" <> line
  pretty (Struct name fields) =
    pretty "struct" <+> pretty name <+> pretty "{" <> line <> indent 2 (vsep (map prettyField fields)) <> line <> pretty "}" <> line

instance Pretty Stmt where
  pretty (SDeclare variable varType) = pretty "var" <+> pretty variable <+> pretty ":" <+> pretty varType <> pretty ";"
  pretty (SInit variable varType expr) = pretty "var" <+> pretty variable <+> pretty ":" <+> pretty varType <+> pretty "="
    <+> pretty expr <> pretty ";"
  pretty (SAssign variable expr) = pretty variable <+> pretty "=" <+> pretty expr <> pretty ";"
  pretty (SAssignAt variable index expr) = pretty variable <> pretty "[" <> pretty index <> pretty "]" <+> pretty "="
    <+> pretty expr <> pretty ";"
  pretty (SRead variable) = pretty "read" <> pretty "(" <> pretty variable <> pretty ")" <> pretty ";"
  pretty (SPrint expr) = pretty "print" <> pretty "(" <> pretty expr <> pretty ")" <> pretty ";"
  pretty (SIf cond thenStmts elseStmts) = pretty "if" <+> pretty "(" <> pretty cond <> pretty ")" <+> pretty "{" <> line
    <> indent 2 (vsep (map pretty thenStmts)) <> line <> pretty "}" <+> pretty "else" <+> pretty "{" <> line
    <> indent 2 (vsep (map pretty elseStmts)) <> line <> pretty "}"
  pretty (SWhile cond whileStmts) = pretty "while" <+> pretty "(" <> pretty cond <> pretty ")" <+> pretty "{" <> line
    <> indent 2 (vsep (map pretty whileStmts)) <> line <> pretty "}"
  pretty (SReturn expr) = pretty "return" <+> pretty expr <> pretty ";"

instance Pretty Exp where
  pretty (EValue val) = pretty val
  pretty (EVar name) = pretty name
  pretty (EVarField var field) = pretty var <> pretty "." <> pretty field
  pretty (ENot e) = pretty "!" <> pretty e
  pretty (EArray values) = pretty "[" <> hsep (punctuate comma (map pretty values)) <> pretty "]"
  pretty (EArrayAt arrayId index) = pretty arrayId <> pretty "[" <> pretty index <> pretty "]"
  pretty (EArrayAtField arrayId index field) = pretty arrayId <> pretty "[" <> pretty index <> pretty "]" <> pretty "." <> pretty field
  pretty (ECall func args) = pretty func <> pretty "(" <> hsep (punctuate comma (map pretty args)) <> pretty ")"
  pretty (EStruct structName args) = pretty structName <> pretty "{" <> hsep (punctuate comma (map pretty args)) <> pretty "}"
  pretty (e1 :<=: e2) = pretty e1 <+> pretty "<=" <+> pretty e2
  pretty (e1 :<: e2) = pretty e1 <+> pretty "<" <+> pretty e2
  pretty (e1 :>=: e2) = pretty e1 <+> pretty ">=" <+> pretty e2
  pretty (e1 :>: e2) = pretty e1 <+> pretty ">" <+> pretty e2
  pretty (e1 :==: e2) = pretty e1 <+> pretty "==" <+> pretty e2
  pretty (e1 :!=: e2) = pretty e1 <+> pretty "!=" <+> pretty e2
  pretty (e1 :||: e2) = pretty e1 <+> pretty "||" <+> pretty e2
  pretty (e1 :&&: e2) = pretty e1 <+> pretty "&&" <+> pretty e2
  pretty (e1 :+: e2) = pretty "(" <> pretty e1 <+> pretty "+" <+> pretty e2 <> pretty ")"
  pretty (e1 :-: e2) = pretty "(" <> pretty e1 <+> pretty "-" <+> pretty e2 <> pretty ")"
  pretty (e1 :*: e2) = pretty "(" <> pretty e1 <+> pretty "*" <+> pretty e2 <> pretty ")"
  pretty (e1 :/: e2) = pretty "(" <> pretty e1 <+> pretty "/" <+> pretty e2 <> pretty ")"

instance Pretty Index where
  pretty (IValue val) = pretty val
  pretty (IVar name) = pretty name
  pretty (IAdd i1 i2) = pretty i1 <+> pretty "+" <+> pretty i2
  pretty (ISub i1 i2) = pretty i1 <+> pretty "-" <+> pretty i2
  pretty IEmpty = pretty ""

instance Pretty Type where
  pretty Int = pretty "int"
  pretty Void = pretty "void"
  pretty Bool = pretty "bool"
  pretty Float = pretty "float"
  pretty String = pretty "string"
  pretty (IntArray index) = pretty "int" <> pretty "[" <> pretty index <> pretty "]"
  pretty (BoolArray index) = pretty "bool" <> pretty "[" <> pretty index <> pretty "]"
  pretty (FloatArray index) = pretty "float" <> pretty "[" <> pretty index <> pretty "]"
  pretty (StructT structName) = pretty structName
  pretty (StructTArray structName index) = pretty structName <> pretty "[" <> pretty index <> pretty "]"