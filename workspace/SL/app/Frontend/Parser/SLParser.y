{
module Frontend.Parser.SLParser (runParser) where

import Utils.Value
import Frontend.Lexer.Token
import Frontend.Lexer.SLLexer hiding (lexer)
import Frontend.Syntax.SLSyntax
}

%name parser SL
%monad {Alex}{(>>=)}{return}
%tokentype { Token }
%error     { parseError }
%lexer {lexer}{Token _ TEOF}

%token
      id       {Token _ (TIdent $$)}
      int      {Token _ (TIntLiteral $$)}
      float    {Token _ (TFloatLiteral $$)}
      str      {Token _ (TStringLiteral $$)}
      'let'    {Token _ TLet}
      '.'      {Token _ TDot}
      ','      {Token _ TComma}
      ':'      {Token _ TColon}
      ';'      {Token _ TSemi}
      'func'   {Token _ TFunc}
      'read'   {Token _ TRead}
      'print'  {Token _ TPrint}
      '('      {Token _ TLParen}
      ')'      {Token _ TRParen}
      '{'      {Token _ TLBrace}
      '}'      {Token _ TRBrace}
      '['      {Token _ TLBracket}
      ']'      {Token _ TRBracket}
      '+'      {Token _ TPlus}
      '-'      {Token _ TMinus}
      '*'      {Token _ TTimes}
      '/'      {Token _ TDivide}
      'if'     {Token _ TIf}
      'else'   {Token _ TElse}
      'while'  {Token _ TWhile}
      'true'   {Token _ TTrue}
      'false'  {Token _ TFalse}
      '!'      {Token _ TNot}
      '||'     {Token _ TOr}
      '&&'     {Token _ TAnd}
      '<='     {Token _ TLtet}
      '<'      {Token _ TLt}
      '>='     {Token _ TGteq}
      '>'      {Token _ TGt}
      '=='     {Token _ TEq}
      '!='     {Token _ TNeq}
      '='      {Token _ TAssign}
      'int'    {Token _ TInt}
      'void'   {Token _ TVoid}
      'bool'   {Token _ TBool}
      'float'  {Token _ TFloat}
      'string' {Token _ TString}   
      'struct' {Token _ TStruct}
      'return' {Token _ TReturn}

%right '='
%left '||'
%left '&&'
%nonassoc '==' '!=' '<' '<=' '>' '>='
%left '+' '-'
%left '*' '/'
%right '!'

%%

SL : Blocks { SL $1 }

Blocks : Block Blocks { $1 : $2 }
       | {- empty -}  { [] }

Block : 'func' id '(' Args ')' ':' Type '{' Stmts '}' { Func $2 $4 $7 $9 }
      | 'struct' id '{' Fields '}'                    { Struct $2 $4 }

Args : id ':' Type ',' Args { ($1, $3) : $5 }
     | id ':' Type          { [($1, $3)] }
     | {- empty -}          { [] }

CallArgs : Exp ',' CallArgs { $1 : $3 }
         | Exp              { [$1] }
         | {- empty -}      { [] }

Fields : id ':' Type ';' Fields { ($1, $3) : $5 }
       | {- empty -}            { [] }

Stmts : Stmt Stmts  { $1 : $2 }
      | {- empty -} { [] }

Stmt : 'let' id ':' Type ';'                               { SDeclare $2 $4 }
     | 'let' id ':' Type '=' Exp ';'                       { SInit $2 $4 $6 }
     | id '=' Exp ';'                                      { SAssign $1 $3 }
     | id '[' Index ']' '=' Exp ';'                        { SAssignAt $1 $3 $6 }
     | 'read' '(' Var ')' ';'                              { SRead $3 }
     | 'print' '(' Exp ')' ';'                             { SPrint $3 }
     | 'if' '(' Exp ')' '{' Stmts '}' 'else' '{' Stmts '}' { SIf $3 $6 $10 }
     | 'if' '(' Exp ')' '{' Stmts '}'                      { SIf $3 $6 [] }
     | 'while' '(' Exp ')' '{' Stmts '}'                   { SWhile $3 $6 }
     | 'return' Exp ';'                                    { SReturn $2 }

Exp : int                   { EValue (VInt $1) }
    | float                 { EValue (VFloat $1) }
    | 'true'                { EValue (VBool True) }
    | 'false'               { EValue (VBool False) }
    | str                   { EValue (VString (read $1)) }
    | Var                   { EVar $1 }
    | Var '.' id            { EVarField $1 $3 }
    | '!' Exp               { ENot $2 }
    | Exp '<=' Exp          { $1 :<=: $3 }
    | Exp '<' Exp           { $1 :<: $3 }
    | Exp '>=' Exp          { $1 :>=: $3 }
    | Exp '>' Exp           { $1 :>: $3 }
    | Exp '==' Exp          { $1 :==: $3 }
    | Exp '!=' Exp          { $1 :!=: $3 }
    | Exp '||' Exp          { $1 :||: $3 }
    | Exp '&&' Exp          { $1 :&&: $3 }
    | Exp '+' Exp           { $1 :+: $3 }
    | Exp '-' Exp           { $1 :-: $3 }
    | Exp '*' Exp           { $1 :*: $3 }
    | Exp '/' Exp           { $1 :/: $3 }
    | '(' Exp ')'           { $2 }
    | '[' Array ']'         { EArray $2 }
    | id '[' Idx ']'        { EArrayAt $1 $3 }
    | id '[' Idx ']' '.' id { EArrayAtField $1 $3 $6 }
    | id '(' CallArgs ')'   { ECall $1 $3 }
    | id '{' CallArgs '}'   { EStruct $1 $3 }

Array : Item ',' Array { $1 : $3 }
      | Item           { [$1] }
      | {- empty -}    { [] }

Item : int     { VInt $1 }
     | float   { VFloat $1 }
     | 'true'  { VBool True }
     | 'false' { VBool False }

Index : Idx         { $1 }
      | {- empty -} { IEmpty }

Idx : int         { IValue (VInt $1) }
    | Var         { IVar $1 }
    | Idx '+' Idx { IAdd $1 $3 }
    | Idx '-' Idx { ISub $1 $3 }

Type : 'int'                 { Int }
     | 'void'                { Void }
     | 'bool'                { Bool }
     | 'float'               { Float }
     | 'string'              { String }
     | 'int' '[' Index ']'   { IntArray $3 }
     | 'bool' '[' Index ']'  { BoolArray $3 }
     | 'float' '[' Index ']' { FloatArray $3 }
     | id                    { StructT $1 [] }
     | id '[' Index ']'      { StructTArray $1 $3 }

Var : id { $1 }

{
parseError (Token (line, col) lexeme)
  = alexError $ "Parse error while processing lexeme: " ++ show lexeme
                ++ "\n at line " ++ show line ++ ", column " ++ show col

lexer :: (Token -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)

runParser :: String -> Either String SL
runParser content = runAlex content parser
}