-- # CompareServer
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
module CompareServer
    ( server
    ) where

import Data.Array
import Data.Bool
import Data.Char
import Data.List
import System.Random.Shuffle
import System.Environment
import System.IO

balls0 :: Int
balls0 = 26

limit0 :: Int
limit0 = 100

server :: IO ()
server = do
    { hSetBuffering stdout LineBuffering 
    ; hSetBuffering stdin LineBuffering
    ; args  <- take 1 <$> getArgs
    ; (b,l) <- case args of
        ["1"] -> pure (26,1000)
        ["2"] -> pure (26,100)
        ["3"] -> pure (5,7)
        _     -> pure (26,100)
    ; let { ?balls = b; ?limit = l }
    ; service
    }

eof :: String
eof = "\EOT"

service :: (?balls :: Int, ?limit :: Int) => IO ()
service = interact . wrap . flip serve res0 =<< initialize
    where
        res0 = unwords (map show [?balls, ?limit])

wrap :: (?limit :: Int) => ([String] -> [String]) -> (String -> String)
wrap f = unlines . (++ [eof]) . f . take (succ ?limit) . lines

serve :: Array Char Int -> String -> ([String] -> [String])
serve aa res reqs = res : serve aa (response aa (take 1 reqs)) (drop 1 reqs)

response :: Array Char Int -> [String] -> String
response aa = \ case
    []    -> "Finished!"
    req:_ -> case words req of
        "?":[x]:[y]:_ -> bool ">" "<" (aa ! x < aa ! y)
        "!":ans:_     -> bool ("WA "++ans0) ("AC "++ans0) (ans0 == ans)
        _             -> "invalid query"
        where
            ans0 = map fst $ sortOn snd $ assocs aa

initialize :: (?balls :: Int) => IO (Array Char Int)
initialize = accumArray (const id) 0 ('A',chr (ord 'A' + pred ?balls)) 
           . zipWith (flip (,)) [1 ..] 
           <$> shuffleM (take ?balls ['A'..'Z'])

