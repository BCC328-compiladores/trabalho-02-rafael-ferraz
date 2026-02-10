module Frontend.Lexer.Token where

-- token definition

data Token
  = Token {
      pos :: (Int, Int)
    , lexeme :: Lexeme
    } deriving (Eq, Ord, Show)

data Lexeme
  = TIntLiteral Int
  | TFloatLiteral Float
  | TDot
  | TComma
  | TLParen
  | TRParen
  | TPlus
  | TMinus
  | TTimes
  | TDivide
  | TIdent { var :: String }
  | TStringLiteral { str :: String }
  | TSemi
  | TColon
  | TInt
  | TVoid
  | TBool
  | TFloat
  | TString
  | TStruct
  | TLet
  | TFunc
  | TRead
  | TPrint
  | TAssign
  | TIf
  | TElse 
  | TWhile
  | TLtet
  | TLt
  | TGteq
  | TGt
  | TEq
  | TNeq
  | TNot
  | TOr
  | TAnd
  | TFalse
  | TTrue
  | TLBrace
  | TRBrace
  | TLBracket
  | TRBracket
  | TReturn
  | TEOF
  deriving (Eq, Ord, Show)