-- # InteractiveSystem

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
module InteractiveSystem
    where

import System.IO
import System.Process

interaction :: (?svrcmd :: String, ?svrargs :: [String])
            => ([String] -> [String]) -> IO ()
interaction iact = do
    { (svrin, svrout) <- prepare ?svrcmd ?svrargs
    ; let { ?svrin = svrin; ?svrout = svrout }
    ; interact' iact 
    }

interact' :: (?svrin :: Handle, ?svrout :: Handle)
          => ([String] -> [String]) -> IO ()
interact' f = hPutStr ?svrin . unlines . f . lines 
            =<< hGetContents ?svrout

prepare :: String -> [String] -> IO (Handle, Handle) 
prepare cmd args = do
    { (Just hin, Just hout, _, _)
        <- createProcess ((proc cmd args)
            { std_in = CreatePipe, std_out = CreatePipe })
        ; hSetBuffering hin LineBuffering
    ; hSetBuffering hout LineBuffering
    ; pure (hin, hout)
    }
