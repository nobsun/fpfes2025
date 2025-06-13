-- # PQ
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
module Dio where

import Control.Arrow
import Control.Monad.Identity
import Control.Monad.RWS.Lazy
import Data.Bool
import Data.Char
import Data.List
import Data.Maybe
import Debug.Trace

tracing :: Show a => a -> a
tracing = trace . show <*> id

-- Dialogue

type Request  = String
type Response = String

type Dialogue = [Response] -> [Request]

type Dio = RWS () [Request] [Response]

getResponse :: Dio Response
getResponse = get >>= (modify (drop 1) >>) . (pure . (!! 0))

putRequest :: Request -> Dio ()
putRequest req = tell [req]

echoClient :: Request -> Dio ()
echoClient req0 = putRequest req0 >> echoLoop

echoLoop :: Dio ()
echoLoop = getResponse >>= \ case
    "\EOT" -> pure ()
    resp   -> putRequest (id resp) >> echoLoop

incServer :: [Request] -> [Response]
incServer = (++ ["\EOT"]) . limiter . map phi where
    phi = show . (succ @Int) . read

resps :: [Response]
resps = incServer reqs

reqs :: [Request]
reqs = limiter $ snd $ evalRWS (echoClient "0") () resps

limiter :: [a] -> [a]
limiter = take 10



