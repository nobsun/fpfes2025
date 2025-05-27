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

import System.Environment
import Debug.Trace qualified as Debug

main :: IO ()
main = do
    { prog <- getProgName
    ; envs <- getEnvironment
    ; args <- getArgs
    ; putStrLn prog
    ; mapM_ print envs
    ; putStrLn $ unwords args
    ; interact proc
    }

proc :: String -> String
proc = unlines . lines

{- debug trace -}
debug :: Bool
debug = () /= ()

trace :: String -> a -> a
trace | debug     = Debug.trace
      | otherwise = const id

tracing :: Show a => a -> a
tracing = trace . show <*> id

{- error -}
impossible :: a
impossible = error "impossible"

invalid :: a
invalid = error "invalid input"
