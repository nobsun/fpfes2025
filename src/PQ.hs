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
module PQ where

import Control.Arrow
import Data.Bool
import Data.Char
import Data.List
import Data.Maybe
import Debug.Trace

tracing :: Show a => a -> a
tracing = trace . show <*> id

-- Dialogue

data Request  = PutStr String | GetStr deriving (Eq, Show)
data Response = OK | OKStr String deriving (Eq,Show)

type Dialogue = [Response] -> [Request]

wrap :: String -> Dialogue -> ([String] -> [String])
wrap q0 dia = (q0 :) . encode . dia . decode
-- wrap q0 dia = encode . ([PutStr q0, GetStr] ++) . dia . decode

decode :: [String] -> [Response]
decode = concatMap phi where
    phi s = [OK, OKStr s]

encode :: [Request] -> [String]
encode = foldr phi [] where
    phi q ss = case q of
        GetStr   -> ss
        PutStr s -> s : ss

-- PQ

newtype PQ a = PQ { action :: [Response] -> (a,[Request],[Response]) }

pqToD :: PQ () -> Dialogue
pqToD io rs = case io.action rs of
    ~(_, qs, _) -> qs

instance Functor PQ where
    fmap :: (a -> b) -> PQ a -> PQ b
    fmap f io = PQ $ \ rs -> case io.action rs of
        ~(x,qs,rs') -> (f x,qs,rs')

instance Applicative PQ where
    pure :: a -> PQ a
    pure x = PQ $ (x,[],)
    (<*>) :: PQ (a -> b) -> (PQ a -> PQ b)
    af <*> ax = PQ $ \ rs -> 
        case af.action rs of
            ~(f,qs1,rs1) -> case ax.action rs1 of
                ~(x,qs2,rs2) -> (f x, qs1 ++ qs2, rs2)

instance Monad PQ where
    (>>=) :: PQ a -> (a -> PQ b) -> PQ b
    mx >>= f = PQ $ \ rs -> 
        case mx.action rs of
            ~(x,qs1,rs1) -> case (f x).action rs1 of
                ~(y,qs2,rs2) -> (y, qs1 ++ qs2, rs2)

donePQ :: PQ ()
donePQ = pure ()

getStrPQ :: PQ String
getStrPQ = PQ $ \ rs -> 
    let str = case rs !! 1 of
            OKStr s -> s
            _       -> error "unexpected response"
    in  (str,[GetStr],drop 2 rs)

putStrPQ :: String -> PQ ()
putStrPQ str = PQ $ \ rs -> ((),[PutStr str],rs)

-- example echoD

echoD :: Dialogue
echoD rs = GetStr : if str == eof
            then []
            else PutStr str : echoD (drop 2 rs)
    where
        str = case rs !! 1 of
            OKStr s -> s
            r1      -> error $ unwords ["unexpected", show r1]

eof :: String
eof = "\EOT"

reqsD :: [String]
reqsD = limiter $ client_ repsD
    where
        client_ = wrap "0" echoD 

repsD :: [String]
repsD = (++ [eof]) $ limiter $ server_ 
    where
        server_ = map (show . succ @Int . read) reqsD

        -- server_ = map phi reqsD
        --     where
        --         phi s   = show @Int (succ $ read @Int s)

countSession :: Int
countSession = 10

mkLim :: Int -> ([a] -> [a])
mkLim = \ case
    0 -> id
    n -> take n

limiter :: [a] -> [a]
limiter = mkLim countSession

-- example echoPQ

echoPQ :: PQ ()
echoPQ = getStrPQ >>= \ str -> if str == eof
    then donePQ
    else putStrPQ str >> echoPQ

reqsPQ :: [String]
reqsPQ = limiter $ client_ repsPQ
    where
        client_ = wrap "0" (pqToD echoPQ)

repsPQ :: [String]
repsPQ = (++ [eof]) $ limiter $ server_ 
    where
        server_ = map (show . succ @Int . read) reqsPQ
