{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{- LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
{-# LANGUAGE CPP #-}
module Main where

import System.Environment
import System.IO
import System.Process
import Control.Monad.Identity
import Data.Bool
import Data.List.Extra
import Data.Tree

import InteractiveSystem
import PQ
import MSort
import SortTree

import Debug.Trace

main :: IO ()
main = do
    { args <- getArgs
    ; let { ?svrcmd = "compare-server"; ?svrargs = take 1 args }
    ; interaction interactiveSort
    }

interactiveSort :: [String] -> [String]
interactiveSort = \ case
    r:rs -> case toTuple $ map (read @Int) $ words r of
        (5,7)     -> sort3 theSortTree rs
        (26,_)    -> encode $ pqToD sortPQ2 $ decode rs
        _         -> error "not yet implemented"
    []   -> error "no inputs"

-- toTuple :: [a] -> (a,a)
-- toTuple = \ case
--     x:y:_ -> (x,y)
--     _     -> error "too short list"

sort3 :: Tree String -> ([String] -> [String])
sort3 t rs = case t of
    Node a []    -> a : trace (rs !! 0) []
    Node q [l,r] -> q : case rs !! 0 of
        "<"              -> sort3 l (drop 1 rs)
        _                -> sort3 r (drop 1 rs)
    _            -> error "impossible!"

sortPQ2 :: PQ ()
sortPQ2 =   sortPQ ['A' .. 'Z'] 
        >>= putStrPQ . fmt 
        >>  getStrPQ 
        >>= flip trace donePQ
    where
        fmt s = unwords ["!", s]

