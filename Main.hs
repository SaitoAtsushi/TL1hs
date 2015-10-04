module Main (main) where
import TL1.Parse
import TL1.Lex
import System.Console.GetOpt
import System.Environment
import Data.Bool
import System.IO
import Text.Parsec.Error

opt :: [OptDescr Bool]
opt = []

main = do
  args <- getArgs
  filename <- return $ optInt args
  inh <- openFile filename ReadMode
  hSetEncoding inh utf8
  str <- hGetContents inh
  case parseTL1 str filename of
    (Left e) -> print e -- $ map messageString $ errorMessages e
    (Right r) -> print r


optInt args =
  case getOpt Permute opt args of
  (_, (n:[]) , _) -> n
  _ -> error "invalid command line arguments"
