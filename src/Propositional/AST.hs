module Propositional.AST (
      Proposition(..)
    , NormalProposition(..)
    , AtomId
    , Propositional(..)
    , ReadableProp(..)) where

class Propositional a where
    normalize :: a -> NormalProposition

type AtomId = String

data Proposition = Atom AtomId
                 | Neg Proposition
                 | And Proposition Proposition
                 | Or Proposition Proposition
                 | Implies Proposition Proposition

data NormalProposition = NormAtom AtomId
                       | NormNegAtom AtomId
                       | NormAnd NormalProposition NormalProposition
                       | NormOr NormalProposition NormalProposition
                       deriving Ord

negationNormalize :: Proposition -> NormalProposition
negationNormalize (Neg (Atom s)) = NormNegAtom s
negationNormalize (Neg (And p q)) = NormOr (negationNormalize $ Neg p) (negationNormalize $ Neg q)
negationNormalize (Neg (Or p q)) = NormAnd (negationNormalize $ Neg p) (negationNormalize $ Neg q)
negationNormalize (Neg (Neg p)) = negationNormalize p
negationNormalize (Neg (Implies p q)) = NormAnd (negationNormalize p) (negationNormalize $ Neg q)
negationNormalize (Atom s) = NormAtom s
negationNormalize (And p q) = NormAnd (negationNormalize p) (negationNormalize q)
negationNormalize (Or p q) = NormOr (negationNormalize p) (negationNormalize q)
negationNormalize (Implies p q) = NormOr (negationNormalize $ Neg p) (negationNormalize q)

instance Propositional NormalProposition where
    normalize = id

instance Propositional Proposition where
    normalize = negationNormalize

instance Eq Proposition where
    p == q = negationNormalize p == negationNormalize q

instance Eq NormalProposition where
    (==) = _eq

_eq :: NormalProposition -> NormalProposition -> Bool
_eq (NormAtom a) (NormAtom b) = a == b
_eq (NormNegAtom a) (NormNegAtom b) = a == b
_eq (NormAnd p q) (NormAnd r s) = _eq p r && _eq q s
_eq (NormOr p q) (NormOr r s) = _eq p r && _eq q s
_eq _ _ = False

instance Show Proposition where
    show (Atom s) = s
    show (Neg p) = "~(" ++ show p ++ ")"
    show (And p q) =  "(" ++ show p ++ " & " ++ show q ++ ")"
    show (Or p q) = "(" ++ show p ++ " | " ++ show q ++ ")"
    show (Implies p q) = "(" ++ show p ++ " -> " ++ show q ++ ")"

instance Show NormalProposition where
    show (NormAtom s) = s
    show (NormNegAtom s) = "~" ++ s
    show (NormAnd p q) =  "(" ++ show p ++ " & " ++ show q ++ ")"
    show (NormOr p q) = "(" ++ show p ++ " | " ++ show q ++ ")"

class (Propositional a) => ReadableProp a where
    readProposition :: String -> Either String a
    readPropositions :: String -> Either String [a]
