{
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Frontend.Lexer.SLLexer where

import Control.Monad
import Frontend.Lexer.Token
}


%wrapper "monadUserState"

$digit = 0-9            -- digits
$letter =[a-zA-Z]      -- letters

-- second RE macros

@int     = $digit+
@float   = $digit+ \. $digit+
@identifier = $letter[$letter $digit]*
@string     = \" ([^\n] # [\" \\] | \\ .)* \"

-- tokens declarations

tokens :-
      -- whitespace and line comments
      <0> $white+       ;
      <0> "//" .*       ;
      -- other tokens
      <0> @int          {mkIntLiteral}
      <0> @float        {mkFloatLiteral}
      <0> "."           {simpleToken TDot}
      <0> ","           {simpleToken TComma}
      <0> ":"           {simpleToken TColon}
      <0> ";"           {simpleToken TSemi}
      <0> "("           {simpleToken TLParen}
      <0> ")"           {simpleToken TRParen}
      <0> "{"           {simpleToken TLBrace}
      <0> "}"           {simpleToken TRBrace}
      <0> "["           {simpleToken TLBracket}
      <0> "]"           {simpleToken TRBracket}
      <0> "+"           {simpleToken TPlus}
      <0> "-"           {simpleToken TMinus}
      <0> "*"           {simpleToken TTimes}
      <0> "/"           {simpleToken TDivide}
      <0> "func"        {simpleToken TFunc}
      <0> "read"        {simpleToken TRead}
      <0> "print"       {simpleToken TPrint}
      <0> "if"          {simpleToken TIf}
      <0> "else"        {simpleToken TElse}
      <0> "while"       {simpleToken TWhile}
      <0> "false"       {simpleToken TFalse}
      <0> "true"        {simpleToken TTrue}
      <0> "<="          {simpleToken TLtet}
      <0> "<"           {simpleToken TLt}
      <0> ">="          {simpleToken TGteq}
      <0> ">"           {simpleToken TGt}
      <0> "=="          {simpleToken TEq}
      <0> "!="          {simpleToken TNeq}
      <0> "="           {simpleToken TAssign}
      <0> "!"           {simpleToken TNot}
      <0> "||"          {simpleToken TOr}
      <0> "&&"          {simpleToken TAnd}
      <0> "let"         {simpleToken TLet}
      <0> "int"         {simpleToken TInt}
      <0> "void"        {simpleToken TVoid}
      <0> "bool"        {simpleToken TBool}
      <0> "float"       {simpleToken TFloat}
      <0> "string"      {simpleToken TString}
      <0> "struct"      {simpleToken TStruct}
      <0> "return"      {simpleToken TReturn}
      <0> @identifier   {mkIdent}
      <0> @string       {mkStringLiteral}
      -- multi-line comment
      <0> "\*"              { nestComment `andBegin` state_comment }
      <0> "*/"              {\ _ _ -> alexError "Error! Unexpected close comment!" }
      <state_comment> "\*"  { nestComment }
      <state_comment> "*/"  { unnestComment }
      <state_comment> .     ;
      <state_comment> \n    ;
{
-- user state

data AlexUserState
  = AlexUserState {
       nestLevel :: Int -- comment nesting level
    }

alexInitUserState :: AlexUserState
alexInitUserState
  = AlexUserState 0

get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)

put :: AlexUserState -> Alex ()
put s' = Alex $ \s -> Right (s{alex_ust = s'}, ())

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f
  = Alex $ \s -> Right (s{alex_ust = f (alex_ust s)}, ())

-- definition of the EOF token

alexEOF :: Alex Token
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  startCode <- alexGetStartCode
  when (startCode == state_comment) $
    alexError "Error: unclosed comment"
  pure $ Token (position pos) TEOF

-- dealing with comments

nestComment :: AlexAction Token
nestComment input len = do
  modify $ \s -> s{nestLevel = nestLevel s + 1}
  skip input len

unnestComment :: AlexAction Token
unnestComment input len
  = do
      s <- get
      let level = (nestLevel s) - 1
      put s{nestLevel = level}
      when (level == 0) $
        alexSetStartCode 0
      skip input len

position :: AlexPosn -> (Int, Int)
position (AlexPn _ x y) = (x,y)

mkIntLiteral :: AlexAction Token
mkIntLiteral (st, _, _, str) len
  = pure $ Token (position st) (TIntLiteral $ read $ take len str)

mkFloatLiteral :: AlexAction Token
mkFloatLiteral (st, _, _, str) len
  = pure $ Token (position st) (TFloatLiteral $ read $ take len str)
mkIdent :: AlexAction Token
mkIdent (st, _, _, str) len
  = pure $ Token (position st) (TIdent (take len str))

simpleToken :: Lexeme -> AlexAction Token
simpleToken lx (st, _, _, _) _
  = return $ Token (position st) lx

mkStringLiteral :: AlexAction Token
mkStringLiteral (st, _, _, str) len
  = pure $ Token (position st) (TStringLiteral (take len str))

-- lexer main function

runLexer :: String -> Either String [Token]
runLexer s = runAlex s go
  where
    go = do
      output <- alexMonadScan
      if lexeme output == TEOF then
        pure [output]
      else (output :) <$> go
}