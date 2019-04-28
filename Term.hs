module Term (Term(..), prettyTerm) where

data Term 
    = Var String
    | App Term Term
    | Abs String Term
    deriving (Show)

prettyTerm :: Term -> String
prettyTerm (Var v)   = v
prettyTerm (Abs p t) = ('\\': p ++ ".") ++ prettyTerm t
prettyTerm (App f a) = '(' : prettyTerm f ++ " " ++ prettyTerm a ++ ")"

