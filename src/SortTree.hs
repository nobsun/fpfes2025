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

buildSortTree :: ([Cand],[Comp]) -> Tree String
buildSortTree = unfoldTree phi where
    phi (ds,ps) = case ds of
        [ans] -> (unwords ["!", showName =<< ans], [])
        _     -> case selectComp (ds,ps) of
            (c,(ls,rs)) -> (query c, [(ls,ps'),(rs,ps')])
                where
                    ps' = delete c ps

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

theSortTree :: Tree String
theSortTree = buildSortTree (initCands 5,initComps 5)
