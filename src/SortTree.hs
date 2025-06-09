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
import Control.Comonad.Trans.Cofree hiding (Cofree, CofreeF (..))
import Control.Comonad.Trans.Cofree qualified as CoF (CofreeF (..))
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

theSortTree :: SortTree ([Cand], [Comp]) String -> Tree String
theSortTree = \ case
    q :< NodeF _ lr -> case lr of
        [] -> Node q []
        _  -> Node q (map theSortTree lr)

the :: Tree String
the = Node {rootLabel = "? A B", subForest = [Node {rootLabel = "? C D", subForest = [Node {rootLabel = "? A C", subForest = [Node {rootLabel = "? C E", subForest = [Node {rootLabel = "? D E", subForest = [Node {rootLabel = "? B D", subForest = [Node {rootLabel = "? B C", subForest = [Node {rootLabel = "! ABCDE", subForest = []},Node {rootLabel = "! ACBDE", subForest = []}]},Node {rootLabel = "? B E", subForest = [Node {rootLabel = "! ACDBE", subForest = []},Node {rootLabel = "! ACDEB", subForest = []}]}]},Node {rootLabel = "? B E", subForest = [Node {rootLabel = "? B C", subForest = [Node {rootLabel = "! ABCED", subForest = []},Node {rootLabel = "! ACBED", subForest = []}]},Node {rootLabel = "? B D", subForest = [Node {rootLabel = "! ACEBD", subForest = []},Node {rootLabel = "! ACEDB", subForest = []}]}]}]},Node {rootLabel = "? A E", subForest = [Node {rootLabel = "? B C", subForest = [Node {rootLabel = "? B E", subForest = [Node {rootLabel = "! ABECD", subForest = []},Node {rootLabel = "! AEBCD", subForest = []}]},Node {rootLabel = "? B D", subForest = [Node {rootLabel = "! AECBD", subForest = []},Node {rootLabel = "! AECDB", subForest = []}]}]},Node {rootLabel = "? B C", subForest = [Node {rootLabel = "! EABCD", subForest = []},Node {rootLabel = "? B D", subForest = [Node {rootLabel = "! EACBD", subForest = []},Node {rootLabel = "! EACDB", subForest = []}]}]}]}]},Node {rootLabel = "? A E", subForest = [Node {rootLabel = "? B E", subForest = [Node {rootLabel = "? B D", subForest = [Node {rootLabel = "? D E", subForest = [Node {rootLabel = "! CABDE", subForest = []},Node {rootLabel = "! CABED", subForest = []}]},Node {rootLabel = "? A D", subForest = [Node {rootLabel = "! CADBE", subForest = []},Node {rootLabel = "! CDABE", subForest = []}]}]},Node {rootLabel = "? D E", subForest = [Node {rootLabel = "? A D", subForest = [Node {rootLabel = "! CADEB", subForest = []},Node {rootLabel = "! CDAEB", subForest = []}]},Node {rootLabel = "? B D", subForest = [Node {rootLabel = "! CAEBD", subForest = []},Node {rootLabel = "! CAEDB", subForest = []}]}]}]},Node {rootLabel = "? A D", subForest = [Node {rootLabel = "? B D", subForest = [Node {rootLabel = "? C E", subForest = [Node {rootLabel = "! CEABD", subForest = []},Node {rootLabel = "! ECABD", subForest = []}]},Node {rootLabel = "? C E", subForest = [Node {rootLabel = "! CEADB", subForest = []},Node {rootLabel = "! ECADB", subForest = []}]}]},Node {rootLabel = "? C E", subForest = [Node {rootLabel = "? D E", subForest = [Node {rootLabel = "! CDEAB", subForest = []},Node {rootLabel = "! CEDAB", subForest = []}]},Node {rootLabel = "! ECDAB", subForest = []}]}]}]}]},Node {rootLabel = "? A D", subForest = [Node {rootLabel = "? D E", subForest = [Node {rootLabel = "? C E", subForest = [Node {rootLabel = "? B C", subForest = [Node {rootLabel = "? B D", subForest = [Node {rootLabel = "! ABDCE", subForest = []},Node {rootLabel = "! ADBCE", subForest = []}]},Node {rootLabel = "? B E", subForest = [Node {rootLabel = "! ADCBE", subForest = []},Node {rootLabel = "! ADCEB", subForest = []}]}]},Node {rootLabel = "? B E", subForest = [Node {rootLabel = "? B D", subForest = [Node {rootLabel = "! ABDEC", subForest = []},Node {rootLabel = "! ADBEC", subForest = []}]},Node {rootLabel = "? B C", subForest = [Node {rootLabel = "! ADEBC", subForest = []},Node {rootLabel = "! ADECB", subForest = []}]}]}]},Node {rootLabel = "? A E", subForest = [Node {rootLabel = "? B D", subForest = [Node {rootLabel = "? B E", subForest = [Node {rootLabel = "! ABEDC", subForest = []},Node {rootLabel = "! AEBDC", subForest = []}]},Node {rootLabel = "? B C", subForest = [Node {rootLabel = "! AEDBC", subForest = []},Node {rootLabel = "! AEDCB", subForest = []}]}]},Node {rootLabel = "? B C", subForest = [Node {rootLabel = "? B D", subForest = [Node {rootLabel = "! EABDC", subForest = []},Node {rootLabel = "! EADBC", subForest = []}]},Node {rootLabel = "! EADCB", subForest = []}]}]}]},Node {rootLabel = "? A E", subForest = [Node {rootLabel = "? B E", subForest = [Node {rootLabel = "? B C", subForest = [Node {rootLabel = "? C E", subForest = [Node {rootLabel = "! DABCE", subForest = []},Node {rootLabel = "! DABEC", subForest = []}]},Node {rootLabel = "? A C", subForest = [Node {rootLabel = "! DACBE", subForest = []},Node {rootLabel = "! DCABE", subForest = []}]}]},Node {rootLabel = "? C E", subForest = [Node {rootLabel = "? A C", subForest = [Node {rootLabel = "! DACEB", subForest = []},Node {rootLabel = "! DCAEB", subForest = []}]},Node {rootLabel = "? B C", subForest = [Node {rootLabel = "! DAEBC", subForest = []},Node {rootLabel = "! DAECB", subForest = []}]}]}]},Node {rootLabel = "? A C", subForest = [Node {rootLabel = "? B C", subForest = [Node {rootLabel = "? D E", subForest = [Node {rootLabel = "! DEABC", subForest = []},Node {rootLabel = "! EDABC", subForest = []}]},Node {rootLabel = "? D E", subForest = [Node {rootLabel = "! DEACB", subForest = []},Node {rootLabel = "! EDACB", subForest = []}]}]},Node {rootLabel = "? C E", subForest = [Node {rootLabel = "! DCEAB", subForest = []},Node {rootLabel = "? D E", subForest = [Node {rootLabel = "! DECAB", subForest = []},Node {rootLabel = "! EDCAB", subForest = []}]}]}]}]}]}]},Node {rootLabel = "? C D", subForest = [Node {rootLabel = "? A D", subForest = [Node {rootLabel = "? A E", subForest = [Node {rootLabel = "? A C", subForest = [Node {rootLabel = "? C E", subForest = [Node {rootLabel = "? D E", subForest = [Node {rootLabel = "! BACDE", subForest = []},Node {rootLabel = "! BACED", subForest = []}]},Node {rootLabel = "! BAECD", subForest = []}]},Node {rootLabel = "? B C", subForest = [Node {rootLabel = "? D E", subForest = [Node {rootLabel = "! BCADE", subForest = []},Node {rootLabel = "! BCAED", subForest = []}]},Node {rootLabel = "? D E", subForest = [Node {rootLabel = "! CBADE", subForest = []},Node {rootLabel = "! CBAED", subForest = []}]}]}]},Node {rootLabel = "? B E", subForest = [Node {rootLabel = "? C E", subForest = [Node {rootLabel = "? B C", subForest = [Node {rootLabel = "! BCEAD", subForest = []},Node {rootLabel = "! CBEAD", subForest = []}]},Node {rootLabel = "? A C", subForest = [Node {rootLabel = "! BEACD", subForest = []},Node {rootLabel = "! BECAD", subForest = []}]}]},Node {rootLabel = "? B C", subForest = [Node {rootLabel = "? A C", subForest = [Node {rootLabel = "! EBACD", subForest = []},Node {rootLabel = "! EBCAD", subForest = []}]},Node {rootLabel = "? C E", subForest = [Node {rootLabel = "! CEBAD", subForest = []},Node {rootLabel = "! ECBAD", subForest = []}]}]}]}]},Node {rootLabel = "? D E", subForest = [Node {rootLabel = "? A E", subForest = [Node {rootLabel = "? B C", subForest = [Node {rootLabel = "! BCDAE", subForest = []},Node {rootLabel = "? B D", subForest = [Node {rootLabel = "! CBDAE", subForest = []},Node {rootLabel = "! CDBAE", subForest = []}]}]},Node {rootLabel = "? B D", subForest = [Node {rootLabel = "? B C", subForest = [Node {rootLabel = "! BCDEA", subForest = []},Node {rootLabel = "! CBDEA", subForest = []}]},Node {rootLabel = "? B E", subForest = [Node {rootLabel = "! CDBEA", subForest = []},Node {rootLabel = "! CDEBA", subForest = []}]}]}]},Node {rootLabel = "? C E", subForest = [Node {rootLabel = "? B E", subForest = [Node {rootLabel = "? B C", subForest = [Node {rootLabel = "! BCEDA", subForest = []},Node {rootLabel = "! CBEDA", subForest = []}]},Node {rootLabel = "? B D", subForest = [Node {rootLabel = "! CEBDA", subForest = []},Node {rootLabel = "! CEDBA", subForest = []}]}]},Node {rootLabel = "? B C", subForest = [Node {rootLabel = "? B E", subForest = [Node {rootLabel = "! BECDA", subForest = []},Node {rootLabel = "! EBCDA", subForest = []}]},Node {rootLabel = "? B D", subForest = [Node {rootLabel = "! ECBDA", subForest = []},Node {rootLabel = "! ECDBA", subForest = []}]}]}]}]}]},Node {rootLabel = "? A C", subForest = [Node {rootLabel = "? A E", subForest = [Node {rootLabel = "? A D", subForest = [Node {rootLabel = "? C E", subForest = [Node {rootLabel = "! BADCE", subForest = []},Node {rootLabel = "? D E", subForest = [Node {rootLabel = "! BADEC", subForest = []},Node {rootLabel = "! BAEDC", subForest = []}]}]},Node {rootLabel = "? B D", subForest = [Node {rootLabel = "? C E", subForest = [Node {rootLabel = "! BDACE", subForest = []},Node {rootLabel = "! BDAEC", subForest = []}]},Node {rootLabel = "? C E", subForest = [Node {rootLabel = "! DBACE", subForest = []},Node {rootLabel = "! DBAEC", subForest = []}]}]}]},Node {rootLabel = "? B E", subForest = [Node {rootLabel = "? D E", subForest = [Node {rootLabel = "? B D", subForest = [Node {rootLabel = "! BDEAC", subForest = []},Node {rootLabel = "! DBEAC", subForest = []}]},Node {rootLabel = "? A D", subForest = [Node {rootLabel = "! BEADC", subForest = []},Node {rootLabel = "! BEDAC", subForest = []}]}]},Node {rootLabel = "? B D", subForest = [Node {rootLabel = "? A D", subForest = [Node {rootLabel = "! EBADC", subForest = []},Node {rootLabel = "! EBDAC", subForest = []}]},Node {rootLabel = "? D E", subForest = [Node {rootLabel = "! DEBAC", subForest = []},Node {rootLabel = "! EDBAC", subForest = []}]}]}]}]},Node {rootLabel = "? C E", subForest = [Node {rootLabel = "? A E", subForest = [Node {rootLabel = "? B C", subForest = [Node {rootLabel = "? B D", subForest = [Node {rootLabel = "! BDCAE", subForest = []},Node {rootLabel = "! DBCAE", subForest = []}]},Node {rootLabel = "! DCBAE", subForest = []}]},Node {rootLabel = "? B C", subForest = [Node {rootLabel = "? B D", subForest = [Node {rootLabel = "! BDCEA", subForest = []},Node {rootLabel = "! DBCEA", subForest = []}]},Node {rootLabel = "? B E", subForest = [Node {rootLabel = "! DCBEA", subForest = []},Node {rootLabel = "! DCEBA", subForest = []}]}]}]},Node {rootLabel = "? D E", subForest = [Node {rootLabel = "? B E", subForest = [Node {rootLabel = "? B D", subForest = [Node {rootLabel = "! BDECA", subForest = []},Node {rootLabel = "! DBECA", subForest = []}]},Node {rootLabel = "? B C", subForest = [Node {rootLabel = "! DEBCA", subForest = []},Node {rootLabel = "! DECBA", subForest = []}]}]},Node {rootLabel = "? B D", subForest = [Node {rootLabel = "? B E", subForest = [Node {rootLabel = "! BEDCA", subForest = []},Node {rootLabel = "! EBDCA", subForest = []}]},Node {rootLabel = "? B C", subForest = [Node {rootLabel = "! EDBCA", subForest = []},Node {rootLabel = "! EDCBA", subForest = []}]}]}]}]}]}]}]}
