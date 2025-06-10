{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module MSort where

import Control.Monad
import Control.Monad.Identity
import Data.Bool
import System.IO

import PQ

msortBy :: Monad m
        => (a -> a -> m Ordering)
        -> [a] -> m [a]
msortBy cmp xs = case halve xs of
    ([],zs) -> pure zs
    (ys,zs) -> merge =<< (,) <$> msortBy cmp ys <*> msortBy cmp zs
    where
        merge = \ case
            ([], bs) -> pure bs
            (as, []) -> pure as
            (aas@(a:as), bbs@(b:bs)) -> do
                { o <- cmp a b
                ; case o of
                    GT -> (b :) <$> merge (aas, bs)
                    _  -> (a :) <$> merge (as, bbs)
                }

halve :: [a] -> ([a],[a])
halve xs = splitAt (length xs `div` 2) xs

msortBy' :: Monad m 
        => (a -> a -> m Ordering) -> [a] -> m [a]
msortBy' cmp = mergeAll <=< sequences where
    sequences = \ case
        a:b:xs -> bool (ascending  b (a:) xs)
                       (descending b [a]  xs)
                       . (GT ==) =<< cmp a b
        xs     -> pure [xs]
    descending a as = \ case
        bs@(b:bs') -> bool (((a:as) :) <$> sequences bs)
                           (descending b (a:as) bs')
                           . (GT ==) =<< cmp a b
        _          -> ((a:as) :) <$> sequences []
    ascending a as = \ case
        bs@(b:bs') -> bool (ascending b (\ ys -> as (a:ys)) bs')
                           (let !x = as [a] in (x : ) <$> sequences bs)
                           . (GT ==) =<< cmp a b
        []         -> let !x = as [a] in (x : ) <$> sequences []
    mergeAll = \ case
        [x] -> pure x
        xs  -> mergeAll =<< mergePairs xs
    mergePairs = \ case
        a:b:xs -> merge a b >>= \ x -> (x :) <$> mergePairs xs
        xs     -> pure xs
    merge = \ case
        as@(a:as') -> \ case
            bs@(b:bs') -> bool ((a :) <$> merge as' bs)
                               ((b :) <$> merge as bs')
                               . (GT ==) =<< cmp a b
            []         -> pure as
        []         -> pure

sortStr :: String -> String
sortStr = runIdentity . msortBy cmpChar 

cmpChar :: Char -> Char -> Identity Ordering
cmpChar x y = pure (compare x y)

cmpCharIO :: Char -> Char -> IO Ordering
cmpCharIO x y = do
    { hPutStrLn stdout (unwords ["?",[x],[y]]) 
    ; hFlush stdout
    ; o <- hGetLine stdin
    ; case o of
        "<" -> pure LT
        _   -> pure GT
    }

sortPQ :: String -> PQ String
sortPQ = msortBy cmpCharPQ

cmpCharPQ :: Char -> Char -> PQ Ordering
cmpCharPQ x y = do
    { putStrPQ (unwords ["?",[x],[y]])
    ; o <- getStrPQ
    ; case o of
        "<" -> pure LT
        _   -> pure GT
    }
