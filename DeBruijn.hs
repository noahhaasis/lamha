module DeBruijn (DeBruijn(..), fromTerm, toTerm, eval) where
import Data.List (elemIndex, find)
import Term

data DeBruijn
    = Bound Int
    | Free String
    | AppI DeBruijn DeBruijn
    | AbsI DeBruijn
    deriving (Show)

fromTerm :: Term -> DeBruijn
fromTerm = fromTerm' []

type Bindings = [String]

fromTerm' :: Bindings -> Term -> DeBruijn
fromTerm' bs (Var v) = case elemIndex v bs of
    Just i -> Bound (i+1)
    Nothing -> Free v
fromTerm' bs (Abs p t) = AbsI $ fromTerm' (p:bs) t
fromTerm' bs (App f a) = AppI (fromTerm' bs f) (fromTerm' bs a)

toTerm :: DeBruijn -> Term
toTerm = toTerm' []

toTerm' :: Bindings -> DeBruijn -> Term
toTerm' _ (Free v)   = Var v
toTerm' b (Bound i)  = Var $ b !! (i-1)
toTerm' b (AppI f a) = App (toTerm' b f) (toTerm' b a)
toTerm' b (AbsI t)   = Abs v (toTerm' (v:b) t)
    where v = uniqueName b

-- Return a unique name which isn't in the list
-- todo: Rewrite this in a way that doesn't include
--       unreachable states/branches
uniqueName :: [String] -> String
uniqueName ns = 
    let isUnique = flip notElem ns
        allNames = concat [map (replicate i) chars | i <- [1..]]
        chars = "abcdefghijklmnopqrstuvwxyz"
    in 
    case find isUnique allNames of
        Just n -> n
        Nothing -> error "unreachable"


-- Substitutes matching Bound vars and decreases the others
subs :: (Int, DeBruijn) -> DeBruijn -> DeBruijn
subs _      (Free v)   = Free v
subs (i, t) (Bound i') = if i == i' then t else if i' > i then Bound (i'-1) else Bound i'
subs b      (AppI f a) = AppI (subs b f) (subs b a)
subs (i, t) (AbsI b)   = AbsI $ subs (i+1, t) b

betaReduce :: DeBruijn -> DeBruijn
betaReduce (AppI (AbsI b) a) = subs (1, a) b
betaReduce t                 = t

eval :: DeBruijn -> DeBruijn
eval (AppI f a) = eval $ betaReduce (AppI (eval f) (eval a))
eval t          = t

