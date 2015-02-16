import Control.Monad
import System.Environment
import Control.Applicative

import Types
import Lexer
import Parser

main :: IO ()
main = do
  content <- (head <$> getArgs) >>= readFile

  putStrLn "Source:"
  putStrLn content

  putStrLn "Tokens:"
  let tokens = alexScanTokens content
  putStrLn . show $ tokens

  let sts = parser tokens
  putStrLn "AST:"
  putStrLn . show $ sts
