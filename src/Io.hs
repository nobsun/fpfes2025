-- # Io
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
module Io where

import Control.Arrow
import Data.Bool
import Data.Char
import Data.List
import Data.Maybe
import Debug.Trace

tracing :: Show a => a -> a
tracing = trace . show <*> id

data Request  = PutStr String | GetStr deriving (Eq, Show)
data Response = OK | OKStr String deriving (Eq,Show)

type Dialogue = [Response] -> [Request]
type Io a = [Response] -> (a,[Request],[Response])

ioToD :: Io () -> Dialogue
ioToD action = \ rs -> case action rs of
    (_, qs, _) -> qs

doneIo :: Io ()
doneIo _ = ((),[],[])

unitIo :: a -> Io a
unitIo v = (v,[],)

bindIo :: Io a -> (a -> Io b) -> Io b
bindIo op fop rs = (v2,qs1 ++ qs2, rs2)
    where
        (v1, qs1, rs1) = op rs
        (v2, qs2, rs2) = fop v1 rs1

seqIo :: Io a -> Io b -> Io b
seqIo m n = m `bindIo` \ _ -> n

wrap :: String -> Dialogue -> ([String] -> [String])
wrap q0 dia = (q0 :) . encode . dia . decode

decode :: [String] -> [Response]
decode = concatMap phi where
    phi s = [OK, OKStr s]

encode :: [Request] -> [String]
encode = foldr phi [] where
    phi q ss = case q of
        GetStr   -> ss
        PutStr s -> s : ss

echo :: Dialogue
echo rs = GetStr : if str == eof
            then []
            else PutStr str : echo (drop 2 rs)
        where
            str = case rs !! 1 of
                OKStr s -> s
                r1       -> error $ unwords ["unexpected", show r1]

eof :: String
eof = "\EOT"

reqs :: [String]
reqs = limiter $ wrap "0" echo reps 

reps :: [String]
reps = (++ [eof]) $ limiter $ map phi reqs
    where
        phi s = show @Int (succ $ read @Int s)

echoIo :: Io ()
echoIo = getStrIo `bindIo` \ str -> if str == eof
    then doneIo
    else putStrIo str `seqIo` echoIo

getStrIo :: Io String
getStrIo rs = (str,[GetStr],drop 2 rs) where
    str = case rs !! 1 of
        OKStr s -> s
        r1      -> error $ unwords ["unexpected", show r1]

putStrIo :: String -> Io ()
putStrIo str rs
    = ((),[PutStr str],rs)

echo' :: Dialogue
echo' = ioToD echoIo

reqs' :: [String]
reqs' = limiter $ wrap "0" echo' reps'

reps' :: [String]
reps' = (++ [eof]) $ limiter $ map phi reqs'
    where
        phi s = show @Int (succ $ read @Int s)

nSess :: Int
nSess = 10

mkLimiter :: Int -> ([a] -> [a])
mkLimiter = \ case
    0 -> id
    n -> take n

limiter :: [a] -> [a]
limiter = mkLimiter nSess


