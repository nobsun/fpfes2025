{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module MSort where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.RWS
import Data.Bool
import System.IO

import Dio

{- -}
sortBy' :: (a -> a -> Ordering) -> [a] -> [a]
sortBy' cmp xs = case ys of
    [] -> zs
    _  -> merge $ (,) (sortBy' cmp ys) (sortBy' cmp zs)
        where
            merge (aas@(a:as), bbs@(b:bs)) = mrg $ cmp a b
                where
                    mrg = \ case
                        GT -> b : merge (aas, bs)
                        _  -> a : merge (as, bbs)
            merge ([], bs) = bs
            merge (as, []) = as
    where
        (ys,zs) = splitAt (length xs `div` 2) xs

-- -}
{- -}
msortBy :: Monad m
        => (a -> a -> m Ordering) -> [a] -> m [a]
msortBy cmp xs = case ys of
    [] -> pure zs
    _  -> merge =<< (,) <$> msortBy cmp ys <*> msortBy cmp zs
        where
            merge (aas@(a:as), bbs@(b:bs)) = mrg =<< cmp a b
                where
                    mrg = \ case
                        GT -> (b :) <$> merge (aas, bs)
                        _  -> (a :) <$> merge (as, bbs)
            merge ([], bs) = return bs
            merge (as, []) = return as
    where
        (ys,zs) = splitAt (length xs `div` 2) xs
-- -}

{- some instances -}

cmpChar :: Char -> Char -> Identity Ordering
cmpChar x y = return (compare x y)

cmpCharIO :: Char -> Char -> IO Ordering
cmpCharIO x y = do
    { hPutStrLn stdout (unwords ["?",[x],[y]]) 
    ; hFlush stdout
    ; o <- hGetLine stdin
    ; case o of
        "<" -> return LT
        _   -> return GT
    }

cmpCharDio :: Char -> Char -> Dio Ordering
cmpCharDio x y =  tell [unwords ["?", [x], [y]]]
               >> (toOrdering <$> getResponse)

toOrdering :: String -> Ordering
toOrdering = \ case
    '<':_ -> LT
    _     -> GT
