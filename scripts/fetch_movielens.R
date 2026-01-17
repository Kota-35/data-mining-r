library(curl)
library(fs)
library(digest)
library(jsonlite)

# 例: "ml-latest-small", "ml-20m", "ml-25m", "ml-32m" など
variant <- "ml-latest-small"
url <- sprintf("https://files.grouplens.org/datasets/movielens/%s.zip", variant)

data_dir <- path("data", "MovieLens")
raw_dir <- path(data_dir, "raw")
man_dir <- path(data_dir, "manifest")
zip_path <- path(raw_dir, paste0(variant, ".zip"))
stamp_path <- path(man_dir, paste0(variant, ".json"))

if (!file_exists(zip_path)) {
  message("Downloading: ", url)
  curl_download(url, destfile = zip_path, quiet = FALSE)
} else {
  message("Zip already exists: ", zip_path)
}

sha256 <- digest(file = zip_path, algo = "sha256")
zip_bytes <- file_info(zip_path)$size

message("Unzipping...")
utils::unzip(zip_path, exdir = raw_dir)

# 展開後のチェック
extracted_root <- path(raw_dir, variant)
if (!dir_exists(extracted_root)) {
  # zip内のフォルダ名が異なる場合もあるのでフォールバック
  extracted_root <- raw_dir
}

csvs <- dir_ls(extracted_root, glob = "*.csv", recurse = TRUE)
line_counts <- setNames(
  lapply(csvs, function(p) length(readLines(p, warn = FALSE))),
  basename(csvs)
)

manifest <- list(
  dataset = "MovieLens",
  variant = variant,
  url = url,
  fetched_at = format(Sys.time(), tz = "Asia/Tokyo"),
  zip = list(
    path = zip_path,
    bytes = zip_bytes,
    sha256 = sha256
  ),
  extracted_root = extracted_root,
  files = list(
    csv = as.character(csvs),
    line_counts = line_counts
  )
)


writeLines(toJSON(manifest, pretty = TRUE, auto_unbox = TRUE), stamp_path)
message("Wrote manifest: ", stamp_path)