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
import Control.Arrow
import Control.Exception
import Control.Monad.Identity
import Control.Monad.State.Lazy
import Data.Bool
import Data.Maybe
import Data.Tree

import Debug.Trace

main :: IO ()
main = do
    {  (hin, hout) <- prepare
    ; let { ?hin = hin ; ?hout = hout }
    ; interact' (foldr (:) [] . ("hogehage" :) . drop 1)
    ; pure ()
    }

solve :: IO_ ()
solve = do
    { res0 <- getResponse
    ; case res0 of
        [msg] -> trace msg loopIO_ "hogehage"
        []    -> trace "Server does not work!" pure ()
        _     -> trace "impossible" pure ()
    }

loopIO_ :: String -> IO_ ()
loopIO_ req = trace req putRequest req >> getResponse >>= dispatch

dispatch :: [String] -> IO_ ()
dispatch = \ case
    []    -> trace "finished!" pure ()
    [res] -> modify (first (drop 1)) >> loopIO_ res
    _     -> trace "impossible!" pure ()

prepare :: IO (Handle, Handle)
prepare = do
    { (Just hin, Just hout, _, _) <-
        createProcess ((shell "upcase-server")
            { std_in = CreatePipe, std_out = CreatePipe })
    ; hSetBuffering hin LineBuffering
    ; hSetBuffering hout LineBuffering
    ; pure (hin, hout)
    }

interact' :: (?hin :: Handle, ?hout :: Handle)
          => ([String] -> [String]) -> IO ()
interact' f = hPutStr ?hin . unlines . f . lines =<< hGetContents ?hout


-- IO_

type Request  = String
type Response = String

type IO_ a = StateT ([Response], Maybe Request) [] a

run :: IO_ () -> ([Response] -> [Request])
run io resps = mapMaybe snd (execStateT io (resps, Nothing))

putRequest :: Request -> IO_ ()
putRequest req = modify (second (const (Just req)))

getResponse :: IO_ [Response]
getResponse = take 1 . fst <$> get

