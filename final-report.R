# install.packages(c("MASS","class","rpart","rpart.plot","e1071","pROC"))

library(MASS)       # Pima.tr / Pima.te
library(class)      # k-NN
library(rpart)      # decision tree
library(rpart.plot) # tree plot
library(e1071)      # SVM
library(pROC)       # ROC/AUC

set.seed(42)

train <- Pima.tr
test  <- Pima.te

# 目的変数（2値）
table(train$type)
table(test$type)

# 説明変数（数値）と目的変数
x_cols <- setdiff(names(train), "type")


eval_binary <- function(y_true, y_pred, positive = levels(y_true)[2]) {
  # y_true, y_pred は factor を想定
  y_true <- factor(y_true)
  y_pred <- factor(y_pred, levels = levels(y_true))
  
  tab <- table(True = y_true, Pred = y_pred)
  
  # 指標計算
  pos <- positive
  neg <- setdiff(levels(y_true), pos)[1]
  
  TP <- tab[pos, pos]
  TN <- tab[neg, neg]
  FP <- tab[neg, pos]
  FN <- tab[pos, neg]
  
  acc <- (TP + TN) / sum(tab)
  prec <- if ((TP + FP) == 0) NA else TP / (TP + FP)
  rec  <- if ((TP + FN) == 0) NA else TP / (TP + FN)
  f1   <- if (is.na(prec) || is.na(rec) || (prec + rec) == 0) NA else 2 * prec * rec / (prec + rec)
  
  list(
    confusion = tab,
    metrics = data.frame(
      accuracy = acc,
      precision = prec,
      recall = rec,
      f1 = f1,
      row.names = NULL
    )
  )
}

# ROC/AUC
eval_roc <- function(y_true, prob_pos, positive = levels(y_true)[2]) {
  roc_obj <- pROC::roc(y_true, prob_pos, levels = rev(levels(y_true)))
  auc_val <- as.numeric(pROC::auc(roc_obj))
  list(roc = roc_obj, auc = auc_val)
}

# 比較用
results <- list()

# 標準化
standardize_train_test <- function(train_df, test_df, cols) {
  mu <- sapply(train_df[, cols, drop = FALSE], mean)
  sdv <- sapply(train_df[, cols, drop = FALSE], sd)
  train_z <- as.data.frame(scale(train_df[, cols, drop = FALSE], center = mu, scale = sdv))
  test_z  <- as.data.frame(scale(test_df[, cols, drop = FALSE],  center = mu, scale = sdv))
  list(train_z = train_z, test_z = test_z)
}

# =========================
# 2) ロジスティック回帰
# =========================
glm_fit <- glm(type ~ ., data = train, family = binomial())

# 予測
glm_prob <- predict(glm_fit, newdata = test, type = "response")
glm_pred <- ifelse(glm_prob >= 0.5, levels(train$type)[2], levels(train$type)[1])
glm_pred <- factor(glm_pred, levels = levels(train$type))

results$logistic <- eval_binary(test$type, glm_pred)

# ROC/AUC
glm_roc <- eval_roc(test$type, glm_prob)
results$logistic$auc <- glm_roc$auc

# 係数
glm_coef <- sort(coef(glm_fit), decreasing = TRUE)
glm_coef

results$logistic

# =========================
# 3) k-NN（kを変えて比較）
# =========================
# kNNは距離ベースなので標準化が
z <- standardize_train_test(train, test, x_cols)
train_z <- z$train_z
test_z  <- z$test_z

k_list <- seq(1, 40, by = 2)

knn_metrics <- data.frame()
knn_confusions <- list()

for (k in k_list) {
  knn_pred <- knn(
    train = train_z,
    test  = test_z,
    cl    = train$type,
    k     = k
  )
  ev <- eval_binary(test$type, knn_pred)
  knn_confusions[[paste0("k=", k)]] <- ev$confusion
  knn_metrics <- rbind(
    knn_metrics,
    data.frame(model = paste0("kNN(k=", k, ")"), ev$metrics)
  )
}

results$knn <- list(metrics_table = knn_metrics, confusion_list = knn_confusions)
results$knn

# kごとの精度比較（図）
acc <- knn_metrics$accuracy

ylim <- range(acc) + c(-0.01, 0.01)
plot(k_list, acc,
     type = "o", pch = 1, lwd = 0.5,
     xlab = "k (odd values)", ylab = "Accuracy",
     ylim = ylim,
     main = "k-NN: Accuracy vs k (Pima)")
grid(nx = NA, ny = NULL, lty = 2)


# =========================
# 4) 決定木（rpart）＋剪定
# =========================
tree_fit <- rpart(type ~ ., data = train, method = "class")

# 交差検証結果
printcp(tree_fit)

cp_tbl <- tree_fit$cptable
min_i <- which.min(cp_tbl[, "xerror"])
min_cp <- cp_tbl[min_i, "CP"]
min_xerr <- cp_tbl[min_i, "xerror"]

# 1-SE ルール（最小xerror + 標準誤差）
xerr_1se <- min_xerr + cp_tbl[min_i, "xstd"]
i_1se <- which(cp_tbl[, "xerror"] <= xerr_1se)[1]   # 最初に閾値を下回る（=より単純な木）
cp_1se <- cp_tbl[i_1se, "CP"]

#cpプロット
plotcp(tree_fit,
       main = "Cost-Complexity Pruning (rpart)",
       lwd = 1.5, cex = 1)

grid(nx = NA, ny = NULL, lty = 3)

# 1-SE の水平線
abline(h = xerr_1se, lty = 2)

# 最小点と1-SE点を強調
points(min_i, cp_tbl[min_i, "xerror"], pch = 19, cex = 1.2)
points(i_1se, cp_tbl[i_1se, "xerror"], pch = 17, cex = 1.2)

legend("topright",
       legend = c(
         paste0("min xerror CP=", signif(min_cp, 3)),
         paste0("1-SE CP=", signif(cp_1se, 3)),
         "1-SE threshold"
       ),
       pch = c(19, 17, NA),
       lty = c(NA, NA, 2),
       bty = "n")

# もっともCV誤差が小さい cp を採用して剪定
best_cp <- tree_fit$cptable[which.min(tree_fit$cptable[, "xerror"]), "CP"]
tree_pruned <- prune(tree_fit, cp = best_cp)

# 予測（剪定前 / 剪定後）
tree_pred <- predict(tree_fit, newdata = test, type = "class")
prun_pred <- predict(tree_pruned, newdata = test, type = "class")

results$tree_raw    <- eval_binary(test$type, tree_pred)
results$tree_pruned <- eval_binary(test$type, prun_pred)

# 木の可視化
rpart.plot(tree_fit, main = "Decision Tree (raw)")
rpart.plot(tree_pruned, main = "Decision Tree (pruned)")

# =========================
# 5) SVM
# =========================
# SVMも標準化した入力を使用
svm_train <- cbind(train_z, type = train$type)
svm_test  <- cbind(test_z,  type = test$type)

svm_fit <- svm(
  type ~ .,
  data = svm_train,
  kernel = "radial",
  cost = 1,
  gamma = 1 / ncol(train_z),
  probability = TRUE
)

svm_pred <- predict(svm_fit, newdata = svm_test, probability = TRUE)
results$svm <- eval_binary(test$type, svm_pred)

# ROC/AUC
svm_prob <- attr(svm_pred, "probabilities")[, levels(train$type)[2]]
svm_roc <- eval_roc(test$type, svm_prob)
results$svm$auc <- svm_roc$auc

results$svm

# =========================
# 6) 結果まとめ
# =========================
summary_table <- rbind(
  data.frame(model = "Logistic", results$logistic$metrics, AUC = results$logistic$auc),
  data.frame(model = "Tree(raw)", results$tree_raw$metrics, AUC = NA),
  data.frame(model = "Tree(pruned)", results$tree_pruned$metrics, AUC = NA),
  data.frame(model = "SVM(RBF)", results$svm$metrics, AUC = results$svm$auc)
)
summary_table

# 混同行列を表示
results$logistic$confusion
results$tree_raw$confusion
results$tree_pruned$confusion
results$svm$confusion

# ROC曲線を同じ図に重ねる
plot(glm_roc$roc,
     main = "ROC curves (Logistic vs SVM)",
     col = "blue",
     lwd = 2,
     print.auc = TRUE)

plot(svm_roc$roc,
     add = TRUE,
     col = "red",
     lwd = 2,
     print.auc = TRUE,
     print.auc.y = 0.4)

legend("bottomright",
       legend = c(
         paste0("Logistic (AUC=", round(results$logistic$auc, 3), ")"),
         paste0("SVM (AUC=", round(results$svm$auc, 3), ")")
       ),
       col = c("blue", "red"),
       lwd = 2,
       bty = "n")