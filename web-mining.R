# ウェブページから情報を抽出する作業を簡略化する機能が多数含まれているパッケージ
install.packages("rvest")

library(rvest)

web.data <- read_html("https://www.shinshu-u.ac.jp/")
# ウェブページのリンク情報を取得
media.link <- html_nodes(web.data, "a")
print(head(media.link))

# リンクタグのテキスト情報を抽出
media.text <- html_text(media.link)
print(head(media.text))

# リンクタグのテキスト情報を抽出
print(head(media.text))

# テキスト情報のデータフレーム化
# htmlから抽出したテキストデータは
# UTF-8でエンコードされているので，
# RMeCabで解析できる（エンコードの変換は必要なし）
media <- as.data.frame(media.text)

# テキスト情報をファイル出力
# 手動で作業用ディレクトリに"web"という名前のディレクトリを作成
write.table(
  media, 
  "./web/link.txt", 
  row.names = FALSE, 
  col.names = FALSE, 
  quote = FALSE
)

# リンクテキストにおける単語の出現頻度の解析
library(RMeCab)
media.freq <- RMeCabFreq("./web/link.txt")
print(head(media.freq)) 

# 名詞と動詞の抽出
media.freq2 <- subset(
  media.freq, 
  Info1 %in% c("名詞", "動詞")
)
print(head(media.freq2)) 

# 数，非自立，接尾の除外
media.freq3 <- subset(
  media.freq2, 
  !Info2 %in% c("数", "非自立", "接尾")
)
print(head(media.freq3)) 

# 単語を出現頻度順にソート
media.freq4 <- media.freq3[
  order(
    media.freq3$Freq,
    decreasing = TRUE
  ), ]
print(head(media.freq4))

# ワードクラウドによる可視化
library(wordcloud) 
set.seed(30)
wordcloud(
  media.freq4$Term, 
  media.freq4$Freq,
  min.freq = 20,
  scale = c(2,.3), 
  color = c(
    "purple", 
    "blue",
    "green", 
    "orange",
    "pink", 
    "red"
    )
)


# 共起ネットワークによる可視化
media.ngram <- NgramDF(
  "./web/link.txt", 
  type = 1,
  pos = c("名詞"), 
  N = 2
)
print(head(media.ngram))

# 出現頻度でソート
media.ngram2 <- media.ngram[
  order(
    media.ngram$Freq,
    decreasing = TRUE
  ), ]
print(head(media.ngram2))

# 共起ネットワークによる可視化
media.ngram2 <- subset(media.ngram2, Freq >= 10)

library(igraph)
media.graph <- graph.data.frame(media.ngram2)
# バイグラムの出現頻度の高いもの(10以上)を指定
plot(
  media.graph, 
  vertex.label = V(media.graph)$name,
  vertex.size = 25
)

