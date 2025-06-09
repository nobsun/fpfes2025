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

import Control.Concurrent
import System.Environment
import Data.Array
import Data.Bool
import Data.List
import Data.List.Extra

import Debug.Trace qualified as Debug

main :: IO ()
main = undefined

server :: String -> [String] -> [String]
server resp0 reqs = resp0 : list [] (server . service) reqs
    where
        service req = case read @Double req of
            p -> show @Int (floor (1.01 * p))

server0 :: [String] -> [String]
server0 = server (unwords ["interest:", show @Double 0.01])

client :: String -> ([String] -> [String])
client req0 resps = req0 : list [] (client . id) resps

reqSequence :: [String]
reqSequence = client "10000000" (drop 1 respSequence)
respSequence :: [String]
respSequence = server0 reqSequence

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
