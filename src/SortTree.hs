-- # SortTree
-- ## 言語拡張と`module`宣言
-- 

{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module SortTree
     where

import Control.Arrow
import Control.Comonad.Cofree
import Control.Comonad.Trans.Cofree hiding (CofreeF (..))
import Control.Comonad.Trans.Cofree as CoF (CofreeF (..))
import Data.Char
import Data.Functor.Base
import Data.Functor.Foldable
import Data.List
import Data.Ord
import Data.Tree

type Cand = [Int]
type Comp = (Int,Int)

initCands :: Int -> [Cand]
initCands n = permutations $ take n [0 ..]

initComps :: Int -> [Comp]
initComps n = map toTuple $ combinations 2 $ take n [0 ..]

type SortTree a = Cofree (TreeF a)

genSortTree :: ([Cand],[Comp]) -> SortTree ([Cand], [Comp]) String
genSortTree = ana psi
    where
        psi = \ case
            (cnds,cmps) -> case cnds of
                [ans] -> ("! " ++ (showName =<< ans)) CoF.:< NodeF (cnds,cmps) []
                _     ->
                    case selectComp (cnds,cmps) of 
                        (c,(ls,rs)) -> query c CoF.:< NodeF (cnds,cmps) [(ls,cmps'), (rs,cmps')]
                            where
                                cmps' = delete c cmps

query :: Comp -> String
query (x,y) = unwords ["?",showName x, showName y]

showName :: Int -> String
showName i = singleton $ chr $ ord 'A' + i

selectComp :: ([Cand],[Comp]) -> (Comp,([Cand],[Cand]))
selectComp = \ case
    ([],_)      -> error "selectComp: empty candidates"
    (_,[])      -> error "selectComp: empty comparing pairs"
    (cnds,cmps) -> minimumBy (comparing phi)
                 $ map ((,) <*> flip prune cnds) cmps
        where
            phi = uncurry max . (length *** length) . snd

prune :: Comp -> [Cand] -> ([Cand],[Cand])
prune (m,n) = partition phi where
    phi c = elemIndices m c < elemIndices n c

combinations :: Int -> [a] -> [[a]]
combinations = \ case
    0   -> const [[]]
    n+1 -> \ case
        []   -> []
        x:xs -> map (x:) (combinations n xs) ++ combinations (n+1) xs
    _   -> error "negative"

toTuple :: [a] -> (a,a)
toTuple = \ case
    x:y:_ -> (x,y)
    _     -> error "too few elems"

theTree :: SortTree ([Cand], [Comp]) String
theTree = genSortTree (initCands 5, initComps 5)

foo :: [String]
foo at = case at of
    a :< NodeF (cands,comps) lrs
        | "!" `isPrefixOf` a -> [a]
        | otherwise          -> case lrs of
            [l,r]                -> a : foo l
