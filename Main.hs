import           Control.Monad (unless)
import           Data.List (notElem)
import           Data.Either.Extra (fromEither)
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
    in fromEither res

read_ :: IO String
read_ = putStr "LC> "
     >> hFlush stdout
     >> getLine

