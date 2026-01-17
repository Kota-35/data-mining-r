# --- データセットの読み込み ---
data_root <- path("data", "MovieLens", "raw", "ml-latest-small")

ratings <- read.csv(
  file = path(data_root, "ratings.csv")
)
movies <- read.csv(
  file = path(data_root, "movies.csv")
)
tags <- read.csv(
  file = path(data_root, "tags.csv")
)
links <- read.csv(
  file = path(data_root, "links.csv")
)

