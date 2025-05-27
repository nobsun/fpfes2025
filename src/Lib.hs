-- # Lib 雛形モジュール
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
module Lib
    ( someFunc
    ) where

-- ## `doctest`のための記述と定義本体
{- | 
「なんか関数」を標準出力に印字する
>>> someFunc
なんか関数
-}
someFunc :: IO ()
someFunc = putStrLn "なんか関数"
