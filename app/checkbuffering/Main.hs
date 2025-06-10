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

import Data.List
import System.IO

main :: IO ()
main = do
    putStrLn . unwords . ("stdin  : " : ) . singleton . show =<< hGetBuffering stdin
    putStrLn . unwords . ("stdout : " : ) . singleton . show =<< hGetBuffering stdout
    putStrLn . unwords . ("stderr : " : ) . singleton . show =<< hGetBuffering stderr
    