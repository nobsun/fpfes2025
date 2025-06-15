{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NPlusKPatterns #-}
module Main where

import System.Environment
import System.IO
import System.Process
import Control.Monad.Identity
import Data.Bool
import Data.Char
import Data.List.Extra
import Data.Tree

import InteractiveSystem
import Dio
import MSort
import SortTree

import Debug.Trace

main :: IO ()
main = do
    { args <- getArgs
    ; let { ?svrcmd = "compare-server"; ?svrargs = take 1 args }
    ; interaction solve
    }

solve :: [String] -> [String]
solve = snd . evalDio interactiveSorting

interactiveSorting :: Dio () 
interactiveSorting = dispatch =<< getBalls
    where
        dispatch = \ case
            5  -> dio (sort3 theSortTree)
            26 -> sort2 26
            _  -> error "unexpected number of balls"

answer :: String -> Dio ()
answer ans =  putRequest (unwords ["!", ans])
           >> getResponse >>= \ r -> trace r return ()

getBalls :: Dio Int
getBalls = read @Int . takeWhile (not . isSpace)
         <$> getResponse 

sort2 :: Int -> Dio ()
sort2 n = answer =<< msortBy cmpCharDio (take n ['A' ..])

sort3 :: Tree String -> ([String] -> [String])
sort3 t rs = case t of
    Node a []    -> a : trace (rs !! 0) []
    Node q [l,r] -> q : case rs !! 0 of
        "<"              -> sort3 l (drop 1 rs)
        _                -> sort3 r (drop 1 rs)
    _            -> error "impossible!"

