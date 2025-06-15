-- # Dio
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

type Dio = RWS () [Request] [Response]

dio :: ([Response] -> [Request]) -> Dio ()
dio t = rws (\ r s -> ((),[],t s))

evalDio :: Dio a -> [Response] -> (a,[Request])
evalDio dio rs = evalRWS dio () rs

getResponse :: Dio Response
getResponse = get >>= (modify (drop 1) >>) . (pure . (!! 0))

getResponse_do :: Dio Response
getResponse_do = do
    { rrs <- get
    ; modify (drop 1)
    ; return (rrs !! 0)
    }

putRequest :: Request -> Dio ()
putRequest req = tell [req]

-- Client / Server 

client :: (Response -> Request) -> Request -> Dio ()
client next req0 =   putRequest req0
                 >>  getResponse
                 >>= \ case
                 "\EOT" -> return ()
                 res    -> client next (next res)

server :: (Request -> Response) -> [Request] -> [Response]
server service = (++["\EOT"]) . limiter . map service

echoClient :: Request -> Dio ()
echoClient = client id

incServer :: [Request] -> [Response]
incServer = server phi where
    phi = show . (succ @Int) . read

resps :: [Response]
resps = incServer reqs

reqs :: [Request]
reqs = limiter $ snd $ evalRWS (echoClient "0") () resps

limiter :: [a] -> [a]
limiter = take 10
