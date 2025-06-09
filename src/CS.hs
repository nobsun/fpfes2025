-- # CS 単純なクライアント/サーバーシステム
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
module CS
    where

type Server  = [Request] -> [Response]
type Service = Request -> Response

server :: Service -> Server
server = map

type Client = [Response] -> [Request]

client :: (Request, Response -> Request) -> Client
client (q0, req) = (q0 :) . map req

-- ## 例

type Request  = Double -- ^ 預金額
type Response = Double -- ^ １年後の元利合計金額

type Rate     = Double -- ^ 年利
type Amount   = Int    -- ^ 金額（１円以下切り捨て）
type Year     = Int    -- ^ 預け期間（年）

nextPandI :: Rate -> (Request -> Response)
nextPandI r = ((1 + r) *)

nextQ :: Response -> Request
nextQ = id

amount :: Rate -> Amount -> (Year -> Amount)
amount r a = floor . (reqs !!) 
    where
        reqs = client (fromIntegral a, nextQ) (server (nextPandI r) reqs)
{- ^
>>> amount 0.01 10000 20
12201
-}
