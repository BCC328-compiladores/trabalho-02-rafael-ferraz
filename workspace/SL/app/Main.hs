module Main where

import Options.Applicative
import Frontend.Lexer.SLLexer as SLLexer
import Frontend.Lexer.Token as Token
import Frontend.Parser.SLParser as SLParser
import Frontend.Syntax.SLSyntax as SLSyntax

-- command line argument parser

data Option
  = Option Exec FilePath

data Exec
  = Lexer | Parser | Pretty 

compilerP :: Parser Option
compilerP = Option <$> execP <*> fileOptionP

fileOptionP :: Parser FilePath
fileOptionP = strOption (  long "file"
                        <> short 'f'
                        <> metavar "FILENAME"
                        <> help "Option file" )

execP :: Parser Exec
execP = flag' (Lexer)
                (  long "Lexer"
                <> help "Use the Lexer")
           <|>
           flag' (Parser)
                 ( long "Parser"
                 <> help "Use the Parser")
           <|>
           flag' Pretty
                 (  long "Pretty"
                 <> help "Use the Pretty")

input :: Parser Option
input = compilerP

opts :: ParserInfo Option
opts = info input
       (fullDesc
       <> header "Lexer, Parser and Pretty Printer")

-- compiler frontend

showLexer :: [Token] -> String
showLexer tokens = unlines (map show tokens)

showParser :: SL -> String
showParser sl = parserTree sl

showPretty :: SL -> String
showPretty sl = prettyPrint sl

frontend :: Exec -> String -> Either String String
frontend Lexer content
  = do
    result <- pure (SLLexer.runLexer content)
    case result of
      Left err -> Left err
      Right tokens -> Right (showLexer tokens)

frontend Parser content
  = do
    result <- pure (SLParser.runParser content)
    case result of
      Left err -> Left err
      Right stmts -> Right (showParser stmts)

frontend Pretty content
  = do
    result <- pure (SLParser.runParser content)
    case result of
      Left err -> Left err
      Right stmts -> Right (showPretty stmts)

-- main expression function.

startPipeline :: Option -> IO ()
startPipeline (Option opt fname)
  = do
      content <- readFile fname
      res <- pure (frontend opt content)
      case res of
        Left errorString -> putStrLn errorString
        Right outputString -> putStr outputString

-- definition of main function

main :: IO ()
main = execParser opts >>= startPipeline