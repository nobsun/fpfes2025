-- # EncServer
-- 
-- ## 言語拡張と`module`宣言

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
module EncServer
    ( server
    ) where

import Data.Array
import Data.Bool
import Data.Char
import Data.List
import System.Random.Shuffle
import System.Environment
import System.IO
import Debug.Trace

balls0 :: Int
balls0 = 26

limit0 :: Int
limit0 = 100

server :: IO ()
server = do
    { hSetBuffering stdout LineBuffering 
    ; service
    }
        
service :: IO ()
service = interact (wrap (serve res0))
    where
        res0 = "abcde"


wrap :: ([String] -> [String]) -> (String -> String)
wrap f = unlines . f . take 10 . lines

serve :: String -> ([String] -> [String])
serve res reqs = res : serve (response (take 1 reqs)) (drop 1 reqs)

response :: [String] -> String
response = \ case
    []    -> trace <*> id $ "Finished!"
    req:_ -> trace req $ bool (map toLower) (map toUpper) (any isLower req) req 
