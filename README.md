# fpfes2025

発表資料：

[『`interact`のすすめ—それでも関数的に書きたいあなたに』](https://github.com/nobsun/fpfes2025/blob/main/doc/markdown/interact.pdf)

## interactive-sort のデモプログラム
```
$ git clone https://github.com/nobsun/fpfes2025.git
$ cd fpfes2025
$ stack build
$ stack exec -- interactive-sort 2 # テストセット番号 2
AC QZICOHAMDLPYKBWUXSNRGVTFEJ      # 提出解 QZICOHAMDLPYKBWUXSNRGVTFEJ 結果 AC
$ stack exec -- interactive-sort 3 # テストセット番号 3
AC EABDC                           # 提出解 EABDC 結果 AC
$ 
```
起動すると、出題側のcompare-serverを起動して、標準入出力を接続するようになっています。

