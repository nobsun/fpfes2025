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
import Control.Monad.Identity
import Data.Bool
import Data.Tree

import Debug.Trace

main :: IO ()
main = do
    {  (hin, hout) <- prepare
    ; let { ?hin = hin ; ?hout = hout }
    ; interact' (run solve)
    }

prepare :: IO (Handle, Handle)
prepare = do
    { as <- take 1 <$> getArgs
    ; bs <- case as of
        ["1"] -> pure as
        ["2"] -> pure as
        ["3"] -> pure as
        _     -> pure ["2"]
    ; (Just hin, Just hout, _, _) <-
        createProcess ((proc "compare-server" bs)
            { std_in = CreatePipe, std_out = CreatePipe })
    ; hSetBuffering hin LineBuffering
    ; hSetBuffering hout LineBuffering
    ; pure (hin, hout)
    }

interact' :: (?hin :: Handle, ?hout :: Handle)
          => ([String] -> [String]) -> IO ()
interact' f = hPutStr ?hin . unlines . f . lines =<< hGetContents ?hout

solve :: IO_ ()
solve = dispatch =<< take 1 . map (read @Int) . words <$> getResponse

dispatch :: [Int] -> IO_ ()
dispatch = \ case
    [5] -> loopIO_ theTree
    _   -> answer =<< msortIO_ (take 26 ['A' ..])

answer :: String -> IO_ ()
answer a = putRequest (unwords ["!", a])

loopIO_ :: Tree String -> IO_ ()
loopIO_ = \ case
    Node q lr -> case lr of
        [l,r] -> putRequest q
            >>   getResponse 
            >>=  bool (loopIO_ l) (loopIO_ r) . ("<" /=)
        _     -> putRequest q

{- --
loop :: (?hin :: Handle, ?hout :: Handle)
     => Tree String -> IO ()
loop = \ case 
    Node q lr -> case lr of
        [l,r] -> do { hPutStrLn ?hin q
                    ; hFlush ?hin
                    ; c <- hGetLine ?hout
                    ; bool (loop l) (loop r) (c == ">")
                    }
        _     -> do { hPutStrLn ?hin q
                    ; hFlush ?hin
                    }
-- -}

-- msort

data Pair a b 
    = Pair
    { fst2 :: a
    , snd2 :: b
    }

pair :: a -> b -> Pair a b
pair = Pair

msortBy :: Monad m => (a -> a -> m Ordering) -> [a] -> m [a]
msortBy cmp xs = case halve xs of
    ([],zs) -> pure zs
    (ys,zs) -> merge =<< Pair <$> msortBy cmp ys <*> msortBy cmp zs
    where
        merge = \ case
            Pair [] bs -> pure bs
            Pair as [] -> pure as
            Pair aas@(a:as) bbs@(b:bs) -> do
                { o <- cmp a b
                ; case o of
                    GT -> (b :) <$> merge (Pair aas bs)
                    _  -> (a :) <$> merge (Pair as bbs)
                }

halve :: [a] -> ([a],[a])
halve xs = splitAt (length xs `div` 2) xs

-- Identity

sortStr :: String -> String
sortStr = runIdentity . msortBy cmpChar 

cmpChar :: Char -> Char -> Identity Ordering
cmpChar x y = pure (compare x y)

-- IO

cmpCharIO :: (?hin :: Handle, ?hout :: Handle) => Char -> Char -> IO Ordering
cmpCharIO x y = do
    { hPutStrLn ?hin (unwords ["?",[x],[y]]) 
    ; hFlush ?hin
    ; o <- hGetLine ?hout
    ; case o of
        "<" -> pure LT
        _   -> pure GT
    }

msortIO :: (?hin :: Handle, ?hout :: Handle) => String -> IO String
msortIO = msortBy cmpCharIO

-- IO_

type Request  = String
type Response = String

data Triple a b c 
    = Triple
    { fst3 :: a
    , snd3 :: b
    , thd3 :: c
    }

data IO_ a = IO_ { action :: [Response] -> Triple a [Request] [Response] }

instance Functor IO_ where
    fmap :: (a -> b) -> IO_ a -> IO_ b
    fmap f io = IO_ 
        $ \ rs -> case action io rs of tri -> tri { fst3 = f (fst3 tri) }

instance Applicative IO_ where
    pure :: a -> IO_ a
    pure x = IO_ $ \ rs -> Triple { fst3 = x, snd3 = [], thd3 = rs }
    (<*>) :: IO_ (a -> b) -> IO_ a -> IO_ b
    af <*> ax = IO_ $ \ rs -> 
        case action af rs of
            tri1 -> case action ax (thd3 tri1) of
                tri2 -> tri2 { fst3 = (fst3 tri1) (fst3 tri2)
                             , snd3 = snd3 tri1 ++ snd3 tri2
                             }

instance Monad IO_ where
    (>>=) :: IO_ a -> (a -> IO_ b) -> IO_ b
    mx >>= f = IO_ $ \ rs -> case action mx rs of
        tri1 -> case action (f (fst3 tri1)) (thd3 tri1) of
            tri2 -> tri2 { snd3 = snd3 tri1 ++ snd3 tri2 }

getResponse :: IO_ Response
getResponse = IO_ $ \ rs ->
    Triple { fst3 = rs !! 0, snd3 = [], thd3 = drop 1 rs }

putRequest :: Request -> IO_ ()
putRequest req
    = IO_ 
    $ \ rs -> Triple { fst3 = (), snd3 = [req], thd3 = rs }

run :: IO_ () -> ([Response] -> [Request])
run io_ rs = case action io_ rs of tri -> snd3 tri

msortIO_ :: String -> IO_ String
msortIO_ = msortBy cmpIO_

cmpIO_ :: Char -> Char -> IO_ Ordering
cmpIO_ x y =  putRequest (unwords ["?",[x],[y]])
           >> (conv <$> getResponse)
    where
        conv = \ case
            "<" -> LT
            _   -> GT

request :: Request -> IO_ ()
request req = IO_ $ \ rs -> Triple { fst3 = (), snd3 = [req], thd3 = rs }

getOrdering :: IO_ Ordering
getOrdering = IO_ $ \ rs -> Triple { fst3 = conv (take 1 rs)
                                   , snd3 = []
                                   , thd3 = drop 1 rs }
    where 
        conv = \ case
            "<":_ -> LT
            _     -> GT

-- sort tree

theTree :: Tree String
theTree = Node {rootLabel = "? A B", subForest = [Node {rootLabel = "? C D", subForest = [Node {rootLabel = "? A C", subForest = [Node {rootLabel = "? C E", subForest = [Node {rootLabel = "? D E", subForest = [Node {rootLabel = "? B D", subForest = [Node {rootLabel = "? B C", subForest = [Node {rootLabel = "! ABCDE", subForest = []},Node {rootLabel = "! ACBDE", subForest = []}]},Node {rootLabel = "? B E", subForest = [Node {rootLabel = "! ACDBE", subForest = []},Node {rootLabel = "! ACDEB", subForest = []}]}]},Node {rootLabel = "? B E", subForest = [Node {rootLabel = "? B C", subForest = [Node {rootLabel = "! ABCED", subForest = []},Node {rootLabel = "! ACBED", subForest = []}]},Node {rootLabel = "? B D", subForest = [Node {rootLabel = "! ACEBD", subForest = []},Node {rootLabel = "! ACEDB", subForest = []}]}]}]},Node {rootLabel = "? A E", subForest = [Node {rootLabel = "? B C", subForest = [Node {rootLabel = "? B E", subForest = [Node {rootLabel = "! ABECD", subForest = []},Node {rootLabel = "! AEBCD", subForest = []}]},Node {rootLabel = "? B D", subForest = [Node {rootLabel = "! AECBD", subForest = []},Node {rootLabel = "! AECDB", subForest = []}]}]},Node {rootLabel = "? B C", subForest = [Node {rootLabel = "! EABCD", subForest = []},Node {rootLabel = "? B D", subForest = [Node {rootLabel = "! EACBD", subForest = []},Node {rootLabel = "! EACDB", subForest = []}]}]}]}]},Node {rootLabel = "? A E", subForest = [Node {rootLabel = "? B E", subForest = [Node {rootLabel = "? B D", subForest = [Node {rootLabel = "? D E", subForest = [Node {rootLabel = "! CABDE", subForest = []},Node {rootLabel = "! CABED", subForest = []}]},Node {rootLabel = "? A D", subForest = [Node {rootLabel = "! CADBE", subForest = []},Node {rootLabel = "! CDABE", subForest = []}]}]},Node {rootLabel = "? D E", subForest = [Node {rootLabel = "? A D", subForest = [Node {rootLabel = "! CADEB", subForest = []},Node {rootLabel = "! CDAEB", subForest = []}]},Node {rootLabel = "? B D", subForest = [Node {rootLabel = "! CAEBD", subForest = []},Node {rootLabel = "! CAEDB", subForest = []}]}]}]},Node {rootLabel = "? A D", subForest = [Node {rootLabel = "? B D", subForest = [Node {rootLabel = "? C E", subForest = [Node {rootLabel = "! CEABD", subForest = []},Node {rootLabel = "! ECABD", subForest = []}]},Node {rootLabel = "? C E", subForest = [Node {rootLabel = "! CEADB", subForest = []},Node {rootLabel = "! ECADB", subForest = []}]}]},Node {rootLabel = "? C E", subForest = [Node {rootLabel = "? D E", subForest = [Node {rootLabel = "! CDEAB", subForest = []},Node {rootLabel = "! CEDAB", subForest = []}]},Node {rootLabel = "! ECDAB", subForest = []}]}]}]}]},Node {rootLabel = "? A D", subForest = [Node {rootLabel = "? D E", subForest = [Node {rootLabel = "? C E", subForest = [Node {rootLabel = "? B C", subForest = [Node {rootLabel = "? B D", subForest = [Node {rootLabel = "! ABDCE", subForest = []},Node {rootLabel = "! ADBCE", subForest = []}]},Node {rootLabel = "? B E", subForest = [Node {rootLabel = "! ADCBE", subForest = []},Node {rootLabel = "! ADCEB", subForest = []}]}]},Node {rootLabel = "? B E", subForest = [Node {rootLabel = "? B D", subForest = [Node {rootLabel = "! ABDEC", subForest = []},Node {rootLabel = "! ADBEC", subForest = []}]},Node {rootLabel = "? B C", subForest = [Node {rootLabel = "! ADEBC", subForest = []},Node {rootLabel = "! ADECB", subForest = []}]}]}]},Node {rootLabel = "? A E", subForest = [Node {rootLabel = "? B D", subForest = [Node {rootLabel = "? B E", subForest = [Node {rootLabel = "! ABEDC", subForest = []},Node {rootLabel = "! AEBDC", subForest = []}]},Node {rootLabel = "? B C", subForest = [Node {rootLabel = "! AEDBC", subForest = []},Node {rootLabel = "! AEDCB", subForest = []}]}]},Node {rootLabel = "? B C", subForest = [Node {rootLabel = "? B D", subForest = [Node {rootLabel = "! EABDC", subForest = []},Node {rootLabel = "! EADBC", subForest = []}]},Node {rootLabel = "! EADCB", subForest = []}]}]}]},Node {rootLabel = "? A E", subForest = [Node {rootLabel = "? B E", subForest = [Node {rootLabel = "? B C", subForest = [Node {rootLabel = "? C E", subForest = [Node {rootLabel = "! DABCE", subForest = []},Node {rootLabel = "! DABEC", subForest = []}]},Node {rootLabel = "? A C", subForest = [Node {rootLabel = "! DACBE", subForest = []},Node {rootLabel = "! DCABE", subForest = []}]}]},Node {rootLabel = "? C E", subForest = [Node {rootLabel = "? A C", subForest = [Node {rootLabel = "! DACEB", subForest = []},Node {rootLabel = "! DCAEB", subForest = []}]},Node {rootLabel = "? B C", subForest = [Node {rootLabel = "! DAEBC", subForest = []},Node {rootLabel = "! DAECB", subForest = []}]}]}]},Node {rootLabel = "? A C", subForest = [Node {rootLabel = "? B C", subForest = [Node {rootLabel = "? D E", subForest = [Node {rootLabel = "! DEABC", subForest = []},Node {rootLabel = "! EDABC", subForest = []}]},Node {rootLabel = "? D E", subForest = [Node {rootLabel = "! DEACB", subForest = []},Node {rootLabel = "! EDACB", subForest = []}]}]},Node {rootLabel = "? C E", subForest = [Node {rootLabel = "! DCEAB", subForest = []},Node {rootLabel = "? D E", subForest = [Node {rootLabel = "! DECAB", subForest = []},Node {rootLabel = "! EDCAB", subForest = []}]}]}]}]}]}]},Node {rootLabel = "? C D", subForest = [Node {rootLabel = "? A D", subForest = [Node {rootLabel = "? A E", subForest = [Node {rootLabel = "? A C", subForest = [Node {rootLabel = "? C E", subForest = [Node {rootLabel = "? D E", subForest = [Node {rootLabel = "! BACDE", subForest = []},Node {rootLabel = "! BACED", subForest = []}]},Node {rootLabel = "! BAECD", subForest = []}]},Node {rootLabel = "? B C", subForest = [Node {rootLabel = "? D E", subForest = [Node {rootLabel = "! BCADE", subForest = []},Node {rootLabel = "! BCAED", subForest = []}]},Node {rootLabel = "? D E", subForest = [Node {rootLabel = "! CBADE", subForest = []},Node {rootLabel = "! CBAED", subForest = []}]}]}]},Node {rootLabel = "? B E", subForest = [Node {rootLabel = "? C E", subForest = [Node {rootLabel = "? B C", subForest = [Node {rootLabel = "! BCEAD", subForest = []},Node {rootLabel = "! CBEAD", subForest = []}]},Node {rootLabel = "? A C", subForest = [Node {rootLabel = "! BEACD", subForest = []},Node {rootLabel = "! BECAD", subForest = []}]}]},Node {rootLabel = "? B C", subForest = [Node {rootLabel = "? A C", subForest = [Node {rootLabel = "! EBACD", subForest = []},Node {rootLabel = "! EBCAD", subForest = []}]},Node {rootLabel = "? C E", subForest = [Node {rootLabel = "! CEBAD", subForest = []},Node {rootLabel = "! ECBAD", subForest = []}]}]}]}]},Node {rootLabel = "? D E", subForest = [Node {rootLabel = "? A E", subForest = [Node {rootLabel = "? B C", subForest = [Node {rootLabel = "! BCDAE", subForest = []},Node {rootLabel = "? B D", subForest = [Node {rootLabel = "! CBDAE", subForest = []},Node {rootLabel = "! CDBAE", subForest = []}]}]},Node {rootLabel = "? B D", subForest = [Node {rootLabel = "? B C", subForest = [Node {rootLabel = "! BCDEA", subForest = []},Node {rootLabel = "! CBDEA", subForest = []}]},Node {rootLabel = "? B E", subForest = [Node {rootLabel = "! CDBEA", subForest = []},Node {rootLabel = "! CDEBA", subForest = []}]}]}]},Node {rootLabel = "? C E", subForest = [Node {rootLabel = "? B E", subForest = [Node {rootLabel = "? B C", subForest = [Node {rootLabel = "! BCEDA", subForest = []},Node {rootLabel = "! CBEDA", subForest = []}]},Node {rootLabel = "? B D", subForest = [Node {rootLabel = "! CEBDA", subForest = []},Node {rootLabel = "! CEDBA", subForest = []}]}]},Node {rootLabel = "? B C", subForest = [Node {rootLabel = "? B E", subForest = [Node {rootLabel = "! BECDA", subForest = []},Node {rootLabel = "! EBCDA", subForest = []}]},Node {rootLabel = "? B D", subForest = [Node {rootLabel = "! ECBDA", subForest = []},Node {rootLabel = "! ECDBA", subForest = []}]}]}]}]}]},Node {rootLabel = "? A C", subForest = [Node {rootLabel = "? A E", subForest = [Node {rootLabel = "? A D", subForest = [Node {rootLabel = "? C E", subForest = [Node {rootLabel = "! BADCE", subForest = []},Node {rootLabel = "? D E", subForest = [Node {rootLabel = "! BADEC", subForest = []},Node {rootLabel = "! BAEDC", subForest = []}]}]},Node {rootLabel = "? B D", subForest = [Node {rootLabel = "? C E", subForest = [Node {rootLabel = "! BDACE", subForest = []},Node {rootLabel = "! BDAEC", subForest = []}]},Node {rootLabel = "? C E", subForest = [Node {rootLabel = "! DBACE", subForest = []},Node {rootLabel = "! DBAEC", subForest = []}]}]}]},Node {rootLabel = "? B E", subForest = [Node {rootLabel = "? D E", subForest = [Node {rootLabel = "? B D", subForest = [Node {rootLabel = "! BDEAC", subForest = []},Node {rootLabel = "! DBEAC", subForest = []}]},Node {rootLabel = "? A D", subForest = [Node {rootLabel = "! BEADC", subForest = []},Node {rootLabel = "! BEDAC", subForest = []}]}]},Node {rootLabel = "? B D", subForest = [Node {rootLabel = "? A D", subForest = [Node {rootLabel = "! EBADC", subForest = []},Node {rootLabel = "! EBDAC", subForest = []}]},Node {rootLabel = "? D E", subForest = [Node {rootLabel = "! DEBAC", subForest = []},Node {rootLabel = "! EDBAC", subForest = []}]}]}]}]},Node {rootLabel = "? C E", subForest = [Node {rootLabel = "? A E", subForest = [Node {rootLabel = "? B C", subForest = [Node {rootLabel = "? B D", subForest = [Node {rootLabel = "! BDCAE", subForest = []},Node {rootLabel = "! DBCAE", subForest = []}]},Node {rootLabel = "! DCBAE", subForest = []}]},Node {rootLabel = "? B C", subForest = [Node {rootLabel = "? B D", subForest = [Node {rootLabel = "! BDCEA", subForest = []},Node {rootLabel = "! DBCEA", subForest = []}]},Node {rootLabel = "? B E", subForest = [Node {rootLabel = "! DCBEA", subForest = []},Node {rootLabel = "! DCEBA", subForest = []}]}]}]},Node {rootLabel = "? D E", subForest = [Node {rootLabel = "? B E", subForest = [Node {rootLabel = "? B D", subForest = [Node {rootLabel = "! BDECA", subForest = []},Node {rootLabel = "! DBECA", subForest = []}]},Node {rootLabel = "? B C", subForest = [Node {rootLabel = "! DEBCA", subForest = []},Node {rootLabel = "! DECBA", subForest = []}]}]},Node {rootLabel = "? B D", subForest = [Node {rootLabel = "? B E", subForest = [Node {rootLabel = "! BEDCA", subForest = []},Node {rootLabel = "! EBDCA", subForest = []}]},Node {rootLabel = "? B C", subForest = [Node {rootLabel = "! EDBCA", subForest = []},Node {rootLabel = "! EDCBA", subForest = []}]}]}]}]}]}]}]}

