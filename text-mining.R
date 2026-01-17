# RMeCabパッケージのインストール
# 事前に https://github.com/ikegami-yukino/mecab/releases よりMecabをインストールしておく必要がある。
# 文字コードは"utf-8"でインストールを実行
install.packages("RMeCab", repos="http://rmecab.jp/R") 
# ワードクラウド
install.packages("wordcloud")
# igraphパッケージのインストール
install.packages("igraph")

# パッケージの読み込み
library(RMeCab)
library(wordcloud)
library(igraph)

# 青空文庫を読み込む関数Aozora
source("https://rmecab.jp/R/Aozora.R")

glove <- Aozora("https://www.aozora.gr.jp/cards/000035/files/301_ruby_5915.zip")
glove.freq <- RMeCabFreq(glove)
print(head(glove.freq))
# 名詞と動詞の抽出
glove.freq2 <- subset(glove.freq, Info1 %in% c("名詞", "動詞"))
# 数，非自立，接尾の除外
glove.freq3 <- subset(glove.freq2, !Info2 %in% c("数", "非自立", "接尾"))
print(head(glove.freq3)) 

# 単語を出現頻度順にソート
glove.freq4 <- glove.freq3[order(glove.freq3$Freq, decreasing = TRUE),]
print(head(glove.freq4))

set.seed(50)
wordcloud(
  glove.freq4$Term,
  glove.freq4$Freq,
  min.freq = 8, # 最低頻度は8
  color = c(
    "purple", 
    "blue",
    "green", 
    "orange",
    "pink", 
    "red"
  )
)

# 共起ネットワーク
# Nグラム解析の実行例
glove.ngram <- NgramDF(
  glove, 
  type = 1, 
  pos = c("名詞"),
  N = 2
)
print(head(glove.ngram))

glove.ngram2 <- glove.ngram[order(glove.ngram$Freq, decreasing = TRUE),]
print(head(glove.ngram2))

# 共起ネットワークの作成
glove.ngram2 <- subset(glove.ngram2, Freq >= 8)
glove.graph <- graph.data.frame(glove.ngram2)
plot(
  glove.graph, 
  vertex.label = V(glove.graph)$name,
  vertex.size = 15
)