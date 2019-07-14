{-# LANGUAGE NamedFieldPuns, LambdaCase #-}

module Propositional.Engine (query, query') where

import Prelude hiding (negate)
import Propositional.AST
import Data.List (partition)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import Debug.Trace

type LiteralMap = Map.Map AtomId Bool
data Branch = Branch {composites:: [NormalProposition], literals:: LiteralMap}

isLiteral :: NormalProposition -> Bool
isLiteral (NormAtom _) = True
isLiteral (NormNegAtom _) = True
isLiteral _ = False

canBranchExpand :: Branch -> Bool
canBranchExpand = not . null . composites 

expandProposition :: NormalProposition -> [[NormalProposition]]
expandProposition (NormAtom s) = [[NormAtom s]]
expandProposition (NormNegAtom s) = [[NormNegAtom s]]
expandProposition (NormAnd a b) = [[a, b]]
expandProposition (NormOr a b) = [[a], [b]] 

_evaluate :: NormalProposition -> (AtomId, Bool)
_evaluate (NormNegAtom s) = (s, False)
_evaluate (NormAtom s) = (s, True)

_updateLiteralMap :: LiteralMap -> [NormalProposition] -> Maybe LiteralMap
_updateLiteralMap m [] = Just m
_updateLiteralMap m (l:ls) = let 
        (atomId, val) = _evaluate l 
    in case Map.lookup atomId m of
        Just b -> if b == val
            then _updateLiteralMap m ls
            else Nothing
        Nothing -> _updateLiteralMap (Map.insert atomId val m) ls

_combine :: LiteralMap -> [NormalProposition] -> [NormalProposition] -> Maybe Branch
_combine literals composites newPs =
    let 
        (literals', composites') = partition isLiteral newPs
        updatedMap = _updateLiteralMap literals literals'
    in
        fmap (\m -> Branch {literals=m, composites=composites ++ composites'}) updatedMap

expandBranch :: Branch -> [Branch]
expandBranch branch@Branch {literals, composites=[]} = [branch]
expandBranch Branch {literals, composites=(c:cs)} = catMaybes $ map (_combine literals cs) $ expandProposition c

tableauSatisfiable :: [Branch] -> Bool
tableauSatisfiable [] = False
tableauSatisfiable (b:bs)
    | canBranchExpand b = tableauSatisfiable (expandBranch b ++ bs)
    | otherwise = True

makeInitBranch :: [NormalProposition] -> Maybe Branch
makeInitBranch ps = 
    let 
        (literals, composites) = partition isLiteral ps
        literalMap = _updateLiteralMap Map.empty literals
    in 
        fmap (\m -> Branch {literals=m, composites}) literalMap

negate :: NormalProposition -> NormalProposition
negate (NormAtom s) = NormNegAtom s
negate (NormNegAtom s) = NormAtom s
negate (NormAnd a b) = NormOr (negate a) (negate b)
negate (NormOr a b) = NormAnd (negate a) (negate b)

query :: (Propositional a, Propositional b) => [a] -> b -> Bool
query kb q = query' (map normalize kb) (normalize q)

query' :: [NormalProposition] -> NormalProposition -> Bool
query' kb q = case makeInitBranch (negate q:kb) of
    Nothing -> True
    Just branch -> not $ tableauSatisfiable [branch]