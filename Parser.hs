module Parser (Term(..), parse) where
import Data.Char (isLetter)
import Data.List (span)

import Term

data Token
    = OpenParenT
    | CloseParenT
    | LambdaT
    | DotT
    | IdentT String
    deriving (Show)

tokenize :: String -> Either String [Token]
tokenize ""       = Right $ []
tokenize (' ':r)  = tokenize r                      -- ignore whitespace
tokenize ('\\':r) = (LambdaT :)     <$> tokenize r
tokenize ('.':r)  = (DotT :)        <$> tokenize r
tokenize ('(':r)  = (OpenParenT :)  <$> tokenize r
tokenize (')':r)  = (CloseParenT :) <$> tokenize r
tokenize s        =
    let splitVar = span isLetter
    in case splitVar s of
        ("", x:xs) -> Left ("Error: Invalid char '" ++ [x] ++ "'")
        (v, r)     -> (IdentT v:) <$> tokenize r

-- todo: Refactor using state
applyFirst :: (a -> c) -> (a, b) -> (c, b)
applyFirst f (a, b) = (f a, b)

parse' :: [Token] -> Either String (Term, [Token])
parse' (LambdaT:IdentT a:DotT:r) = applyFirst (Abs a) <$> parse' r  -- Abstraction
parse' (IdentT v:r)   = Right $ (Var v, r)                          -- Variable
parse' (OpenParenT:r) = do
    (f, r) <- parse' r
    (a, r) <- parse' r
    case r of
        (CloseParenT:r') -> Right $ (App f a, r')
        (t:_)           -> unexpected t
        []              -> Left ("Expected closing parentheses")
parse' (t:ts)            = unexpected t

parse :: String -> Either String Term
parse s = case (parse' =<< tokenize s) of
    Right (t, [])   -> Right $ t
    Right (_, t:ts) -> unexpected t
    Left e          -> Left e

unexpected t = Left ("Unexpected token " ++ show t)

