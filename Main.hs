import           Control.Monad (unless)
import           Data.List (notElem)
import           System.IO (hFlush, stdout)

import qualified DeBruijn
import           DeBruijn (DeBruijn(..))
import           Parser
import           Term

main :: IO ()
main = repl

repl :: IO ()
repl = do
  input <- read_
  
  unless (input == ":quit")
       $ putStrLn (eval input) >> repl

eval :: String -> String
eval s = 
    let term = parse s
        deBruijn = DeBruijn.eval <$> (DeBruijn.fromTerm <$> term)
        res = (prettyTerm . DeBruijn.toTerm) <$> deBruijn
    in case res of
        Right s -> s
        Left  e -> e

read_ :: IO String
read_ = putStr "LC> "
     >> hFlush stdout
     >> getLine

