{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
{-# LANGUAGE CPP #-}

module Main where

import Control.Arrow
import Data.Char
import Data.List
import System.IO

main :: IO ()
main = do
    putStrLn . unwords . ("stdin  : " : ) . singleton . show =<< hGetBuffering stdin
    putStrLn . unwords . ("stdout : " : ) . singleton . show =<< hGetBuffering stdout
    putStrLn . unwords . ("stderr : " : ) . singleton . show =<< hGetBuffering stderr

newtype St s a = St { runSt :: s -> (a,s) }
instance Functor (St s) where
    fmap :: (a -> b) -> St s a -> St s b
    fmap f px  = St $ \ s -> case px.runSt s of
        (x,s') -> (f x, s')

instance Applicative (St s) where
    pure :: a -> St s a
    pure x = St $ \ s -> (x,s)
    (<*>) :: St s (a -> b) -> St s a -> St s b
    St phi <*> St psi  = St $ \ s -> case phi s of
        (f,s') -> case psi s' of
            (x,s'') -> (f x, s'')

instance Monad (St s) where
    (>>=) :: St s a -> (a -> St s b) -> St s b
    sx >>= f = sx { runSt = \ s -> case sx.runSt s of
        (x,s') -> (f x).runSt s'}
