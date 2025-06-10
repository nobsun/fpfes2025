---
marp: true
style: |
  .title h1 {
    text-align: center;
    line-height: 1.6em;
    font-size: 2em;
  }
  .info {
    text-align: center;
    margin-top: 1.6em;
  }
  section {
    font-family: 'Migu 1C';
  }
  p br {
    display: none;
  }
  code, pre {
    font-family: 'HackGen Console NF';
    font-size: 90%;
  }
  li {
    font-size: 100%;
  }
  table {
    /* font-size: 18px; */
  }
  div.mermaid {
    all: unset;
  }
theme: default
paginate: true
math: katex
---

<div class="title">

# `interact`のすすめ

</div>
<div class="info">

## それでも関数的に書きたいあなたに

### 2025-06-14
### 関数型まつり2025
### 山下伸夫 @ SampouOrg

</div>

---
## 目次

1. モチベーション：関数的に書きたい
   - 「関数的」のお気持ち
2. お題：
   - AtCoder提出プログラムの雛形
   - 状態遷移系のステップ実行
   - [Interactive Sorting](https://atcoder.jp/contests/practice/tasks/practice_2)
3. まとめ：
   - Laziness

---
## モチベーション
$$
\;
$$

```
main :: IO ()
```
$$
\;\\
\;
$$
> おおもとが「関数」ではなく「命令」！？
>
> 関数的プログラミングがしたい

---
## モチベーション

「関数」を「命令」にしてくれる関数

```haskell
interact :: (String -> String) -> IO ()
```
を使って
```haskell
someFunc :: String -> String
```
をプログラムしよう
```haskell
main = interact someFunc
```

---
## 関数的（functional）

> **Functional**(also called **right-unique** or **univalent**): for all $x \in X$ and all $y,z \in Y$, if $xRy$ and $xRz$ then $y=z$.
>
> [WikiPedia: Binary relation](https://en.wikipedia.org/wiki/Binary_relation)

---
## 「関数的」のお気持

| | 関数的 | 命令的 |
|--|--|--|
|思考|トップダウン|ボトムアップ|
|時間|静的|動的|
|状態|明示的|暗黙的|
|型付|静的|動的|
|計算|関数適用|命令実行|
|区切|関数合成演算子`.`|命令区切り子`;`|

フォン・ノイマン型計算機を前提としない空想の世界

---
## 関数型（function type）

`a -> b`： `a`型の値に適用すると`b`型の値になる「関数」の型

> $\sigma\;および\;\tau\;が型ならば\;\sigma \rightarrow \tau\;は型$

---
## 命令型（instruction type）

`IO a` ： `a`型の値を得る「命令」の型

$$
\;
$$

**N.B.** 「命令型」は、この資料独自の（オレオレ）用語法

---
## Haskellプログラミング $\ne$ 関数プログラミング

Haskellでは、命令的に書ける

```haskell
main :: IO ()
main = do 
    { inp <- getContents
    ; let xs = map (read @Int) $ words inp
    ; let ys = scanl1 (+) xs
    ; forM_ ys print
    }
```
---
## お題： AtCoder提出プログラムの雛形

[Welcome to AtCoder](https://atcoder.jp/contests/practice/tasks/practice_1)

> **入力**
> 
> 入力は以下の形式で与えられる
> ```
> a
> b c
> s
> ```
> **出力**
> 
> $a+b+c$ と $s$ を空白区切りで1行に出力せよ。

---
```haskell
type I = String
type O = String

type Dom   = ([I],I)
type Codom = [O]

type Solver = Dom -> Codom

solve :: Solver
solve = \ case
  (abc,s) -> [show $ sum $ read @Int <$> abc, s]

main :: IO ()
main = interact (detokenize . encode . solve . decode . entokenize)
       --------
```
(つづく)

---
(つづき)
```haskell
decode :: [[I]] -> Dom
decode = \ case
  [a]:[b,c]:[s]:_ -> ([a,b,c], s)
  _               -> error "invalid input"

encode :: Codom -> [[O]]
encode = \ case
  rs -> [rs]

entokenizer :: AsToken a => String -> [[a]]
entokenizer = map read . lines

detokenizer :: AsToken a => [[a]] -> String
detokenizer = unlines . map show
```
---

---
## 対話（Dialogue）

対話：メッセージのやりとりの反復

典型例：クライアント・サーバー系
  - サーバー：    要求列 → 応答列
  - クライアント：応答列 → 要求列

---
```haskell
type Server  = [Request] -> [Response]

type Client = [Response] -> [Request]

server :: Server
server = map service

service :: Request -> Response
service = ..

client :: Request -> Client
client req0 = (req0 :) . map query

query :: Response -> Request
query 
```

---
```
                                 req0
                                  ↓
┌────────────┐     resps     ┌────────────┐
│            +——————————————>+            │
│   Server   │               │   Client   │
│            +<——————————————+            │
└────────────┘   req0:reqs   └────────────┘
```
---
```haskell
type Request = Double; type Response = Double
type Rate    = Double; type Amount   = Int   ; type Year = Int

process :: Request -> Response
process = ((1 + rate) *)

rate :: Rate 
rate = 0.01

next :: Response -> Request
next = id

amount :: Rate -> Amount -> (Year -> Amount)
amount r a = floor . (reqs !!) 
    where
        reqs = client req0 (server process reqs)
        req0 = fromIntegral a
{- ^
>>> amount 0.01 10000 20
12201
-}
```
---
## 外界との対話
```haskell
main :: IO ()
main = interact (encode . client req0 . decode)
```

```
外界         │
             │            resps    ┌─────────────┐
    String → + → decode —————————> +             │
             │                     │ Client req0 │
    String ← + ← encode <————————— +             │
             │           req0:reqs └─────────────┘
```

<!--
---
```
┌─┼─┐  u+250C u+2500 u+2510
│   │  u+2502        u+2502
└─┼─┘  u+2514 u+2500 u+2518
```
-->
---
```haskell
decode :: String -> [Responses]
decode = map dec . lines
{- lines :: String -> [String] -}
{- dec   :: String -> Response -}

encode :: [Request] -> String
encode = unlines . map enc
{- enc     :: Request  -> String -}
{- unlines :: [String] -> String -}
```
---
## 関数型の拡張

- `a -> b` を対話を含むように拡張
  ```haskell
  (a, [Response]) -> (b, [Request],[Response])
  ```
- カリー化
  ```haskell
  a -> ([Response] -> (b, [Request],[Response])
  ```
- 対話部分を抽象 `type IO_ b = [Response] -> (b,[Request],[Response])`
  ```haskell
  a -> IO_ b
  ```
- `IO_ b` の解釈： 対話から得られる`b`型の値

---
## 値の対話への埋め込み

```haskell
unitIO_ :: a -> IO_ a
unitIO v ps = (v, [], ps)
```

---
## 関数合成の拡張

```haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) x = f (g x)

(<--) :: (b -> IO_ c) -> (a -> IO_ b) -> (a -> IO_ c)
(f <-- g) x ps = case g x ps of
    (y,qs,ps')        -> case f y ps' of
        (z,qs',ps'')      -> (a,qs ++ qs', ps'')
```

---
## 対話の起動
```haskell
run :: IO_ a -> ([Response] -> [Request])
run action rs = case action rs of
    (_, qs, _) -> qs
```

