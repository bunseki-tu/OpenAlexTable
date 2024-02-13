# OpenAlexTable

[OpenAlex](https://openalex.org/)のAPIを利用して複数機関の数字をまとめて取得してテーブル表示するツールです。  
OpenAlexのAPIおよび[openalexR](https://docs.ropensci.org/openalexR/)について手を動かして知ろうとしたらできあがりました。  
誰かの何かの参考になるかもしれないので公開します。  

# Requirement

- R
- tidyverse
- shiny
- DT
- openalexR

OpenAlexのAPIにアクセスするのでオンライン環境下でないと動きません。

# Usage

OpenAlexTable.Rを[Rstudio](https://posit.co/products/open-source/rstudio/)で開いたら，Run Appボタンを押してください。ツールが起動します。

測りたい指標（Indicator），数字を知りたい大学（University），出版年の範囲（Publication Year），文献タイプ（Works Type）を選択したら，Calcボタンを押してください。右にテーブルが表示されます。  
また，テーブルの上に表示されるCSVなどのボタンを押すと，テーブルをダウンロードしたりすることができます。


# Note

OpenAlexのAPIにアクセスするので，APIのリミットには注意してください。
https://docs.openalex.org/how-to-use-the-api/rate-limits-and-authentication

# Author

* mwada(c8)
* Research Management Center, Office of Research Promotion, Tohoku University

# License

OpenAlexTable is under [MIT license](https://en.wikipedia.org/wiki/MIT_License).  
本リポジトリ内で公開しているコードを利用する際は，各自の責任でご使用ください。
