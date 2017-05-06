# --------------------------------------------------------------------------------
# This is Carvana File 1.  Run this first to obtain predictions from base models. 
# --------------------------------------------------------------------------------

# --------------------------
# Install and load packages.
# --------------------------

# install.packages("doSNOW")
# install.packages("foreach")
# install.packages("parallel")
# install.packages("glmnet")
# install.packages("randomForest")
# install.packages("xgboost")
# install.packages("cvTools")
# install.packages("Matrix")
# install.packages("lubridate")

# For parallel processing on a Windows machine.
library(doSNOW)
library(foreach)
library(parallel)

# Statistical or machine learning packages.
library(glmnet)
library(randomForest)
library(xgboost)

# Other packages.
library(cvTools)  # For cross-validation
library(Matrix)  # For sparse.model.matrix function.
library(lubridate) # For feature engineering.


# -------------------------------
# Set up for parallel processing.
# -------------------------------

# Set up your multiple cores as separate workers and then make them a cluster.
workers <- detectCores()
cluster <- makeCluster(workers, type = "SOCK")
registerDoSNOW(cluster)


# -----------------------------
# Load the data and inspect it. 
# -----------------------------

train.df <- read.csv("training.csv", na.strings = c("", "NULL") , stringsAsFactors = TRUE)
head(train.df)
mean(train.df$IsBadBuy)
dim(train.df)


# ---------------
# Clean the data.
# ---------------

# Manual transmissions appears as either "Manual" or "MANUAL".  Convert all to "Manual".
train.df$Transmission[train.df$Transmission == "Manual"] <- "MANUAL"
train.df$Transmission <- as.factor(as.character(train.df$Transmission))
summary(train.df$Transmission)

# Add variables for week, month, and year.
train.df$PurchDate <- as.Date(train.df$PurchDate, format = "%m/%d/%Y")
train.df$PurchWeek <- week(train.df$PurchDate)
train.df$PurchMonth <- month(train.df$PurchDate)
train.df$PurchYear <- year(train.df$PurchDate)

# Convert some variables into factors.
factor.variables <- c(4, 7:14, 16:18, 27:31, 33, 35:37)
for(i in factor.variables) {
  train.df[, i] <- factor(train.df[, i], exclude = NULL)
}

# Data imputation.
ImputeData <- function(vec, mn) {
  ifelse(is.na(vec), mn, vec)
}
data.mean <- sapply(train.df[ , 19:26], mean, na.rm = TRUE)
for(i in 19:26) {
  train.df[, i] <- ImputeData(train.df[ , i], data.mean[i - 18])
}

# Examine the results.
train.summary.df <- data.frame("Variable" = seq(1, dim(train.df)[2]), 
                               "Class" = sapply(train.df, class), 
                               "NA Count" = colSums(sapply(train.df, is.na)), 
                               "nLevels" = sapply(train.df, nlevels))

# ---------------------------
# Partition and process data.
# ---------------------------

selected.variables <- c(4:37)
xtrain <- train.df[, selected.variables]

xtrain.summary.df <- data.frame("Variable" = seq(1, dim(xtrain)[2]), 
                                "Class" = sapply(xtrain, class), 
                                "NA Count" = colSums(sapply(xtrain, is.na)), 
                                "nLevels" = sapply(xtrain, nlevels))

# For the lasso.
xtrain.lasso <- sparse.model.matrix(~ 0 + ., data = train.df[, selected.variables])
ytrain.lasso <- as.vector(train.df[, 2])

# For the random forest. Convert the factors with levels > 32 to numeric.
xtrain.rf <- train.df[, selected.variables]
for(i in c(4:7, 26:28, 32)) {
  levels(xtrain.rf[, i]) <- seq(1, length(levels(xtrain.rf[, i])))
  xtrain.rf[, i] <- as.numeric(levels(xtrain.rf[, i]))[xtrain.rf[, i]]
}
# Convert NA to "NA" for random forest.
for(i in c(8:11, 13:15, 24, 25, 30)) {
  levels(xtrain.rf[, i])[is.na(levels(xtrain.rf[, i]))] <- "NA"
}
ytrain.rf <- as.vector(train.df[, 2])

xtrain.rf.summary.df <- data.frame("Variable" = seq(1, dim(xtrain.rf)[2]), 
                              "Class" = sapply(xtrain.rf, class), 
                              "NA Count" = colSums(sapply(xtrain.rf, is.na)), 
                              "nLevels" = sapply(xtrain.rf, nlevels))

# For the xgboost.
xtrain.xgb <- sparse.model.matrix(~ 0 + ., data = train.df[, selected.variables])
ytrain.xgb <- as.vector(train.df[, 2])


# ----------------------
# Set up a scoring rule.
# ----------------------

log.loss <- function(pred, real) {
  return(-mean(real * log(pred) + (1 - real) * log(1 - pred)))
}


# -----------------------------------
# Generate out-of-sample predictions.
# -----------------------------------

ntotal <- nrow(xtrain)
nfolds <- 10
set.seed(201)
folds <- cvFolds(ntotal, K = nfolds)

for(i in 1:nfolds) {
  
  held_out_fold <- i
  
  xtrainsmall.lasso <- xtrain.lasso[folds$subsets[folds$which != held_out_fold], ]
  ytrainsmall.lasso <- ytrain.lasso[folds$subsets[folds$which != held_out_fold]]
  xvalid.lasso <- xtrain.lasso[folds$subsets[folds$which == held_out_fold], ]
  yvalid.lasso <- ytrain.lasso[folds$subsets[folds$which == held_out_fold]]
  
  xtrainsmall.rf <- xtrain.rf[folds$subsets[folds$which != held_out_fold], ]
  ytrainsmall.rf <- ytrain.rf[folds$subsets[folds$which != held_out_fold]]
  xvalid.rf <- xtrain.rf[folds$subsets[folds$which == held_out_fold], ]
  yvalid.rf <- ytrain.rf[folds$subsets[folds$which == held_out_fold]]
  
  xtrainsmall.xgb <- xtrain.xgb[folds$subsets[folds$which != held_out_fold], ]
  ytrainsmall.xgb <- ytrain.xgb[folds$subsets[folds$which != held_out_fold]]
  xvalid.xgb <- xtrain.xgb[folds$subsets[folds$which == held_out_fold], ]
  yvalid.xgb <- ytrain.xgb[folds$subsets[folds$which == held_out_fold]]
  
  # Run a regularized logistic regression.  
  ptm <- proc.time()
  set.seed(201)
  lasso.model.cv <- cv.glmnet(xtrainsmall.lasso, ytrainsmall.lasso, family = "binomial", parallel = TRUE)
  proc.time() - ptm
  lasso.model.cv.pred <- predict(lasso.model.cv, newx = xvalid.lasso, s = "lambda.min", type = "response") 
  log.loss(lasso.model.cv.pred, yvalid.lasso)
  
  # Run a random forest.  
  ptm <- proc.time()
  rf <- foreach(j = 1:workers, .combine=combine, .multicombine=TRUE, .inorder=FALSE, .packages = c("randomForest")) %dopar% {
    set.seed(j)
    randomForest(xtrainsmall.rf, ytrainsmall.rf, sampsize = 20000, ntree = ceiling(504/workers))
  }
  proc.time() - ptm
  test.prob.rf <- predict(rf, xvalid.rf)
  min.test.prob.rf <- min(test.prob.rf[test.prob.rf > 0])
  log.loss(ifelse(test.prob.rf <= 0, min.test.prob.rf, test.prob.rf), yvalid.rf)
  test.prob.rf[1:100]
  
  # Run xgboost.  
  cv_for_xgb_on <- 1
  if (cv_for_xgb_on) {
    param <- list(objective = "binary:logistic")
    set.seed(201)
    ptm <- proc.time()
    xgb.trees.cv <- xgb.cv(xtrainsmall.xgb, ytrainsmall.xgb, params = param, nthread = workers, 
                           nfold = 10, nrounds = 200)
    proc.time() - ptm
    plot(xgb.trees.cv$evaluation_log$train_error_mean, ylim=c(0.09,0.12))
    lines(xgb.trees.cv$evaluation_log$test_error_mean)
    min(xgb.trees.cv$evaluation_log$test_error_mean)
    nrounds.cv = which(xgb.trees.cv$evaluation_log$test_error_mean == min(xgb.trees.cv$evaluation_log$test_error_mean))[1]
  }

  # Fit the best xgboost model.
  ptm <- proc.time()
  set.seed(201)
  xgb.trees <- xgboost(xtrainsmall.xgb, ytrainsmall.xgb, nround = nrounds.cv, objective = "binary:logistic")
  proc.time() - ptm 
  test.prob.xgb <- predict(xgb.trees, xvalid.xgb)
  log.loss(test.prob.xgb, yvalid.xgb)
  
  # Write out predictions, models, and data.  
  test.prob.rlr <- lasso.model.cv.pred
  test.prob.rf <- test.prob.rf 
  test.prob.xgb <- test.prob.xgb 
  test.prob.df <- data.frame("rlrprob" = as.numeric(test.prob.rlr), 
                             "rfprob" = test.prob.rf, 
                             "xgbprob" = test.prob.xgb, 
                             "IsBadBuy" = yvalid.xgb)
  if (held_out_fold == 1) write.csv(test.prob.df, "IsBadBuy-oos-prob.df1.csv", row.names = FALSE)
  if (held_out_fold == 2) write.csv(test.prob.df, "IsBadBuy-oos-prob.df2.csv", row.names = FALSE) 
  if (held_out_fold == 3) write.csv(test.prob.df, "IsBadBuy-oos-prob.df3.csv", row.names = FALSE)
  if (held_out_fold == 4) write.csv(test.prob.df, "IsBadBuy-oos-prob.df4.csv", row.names = FALSE)
  if (held_out_fold == 5) write.csv(test.prob.df, "IsBadBuy-oos-prob.df5.csv", row.names = FALSE)
  if (held_out_fold == 6) write.csv(test.prob.df, "IsBadBuy-oos-prob.df6.csv", row.names = FALSE)
  if (held_out_fold == 7) write.csv(test.prob.df, "IsBadBuy-oos-prob.df7.csv", row.names = FALSE)
  if (held_out_fold == 8) write.csv(test.prob.df, "IsBadBuy-oos-prob.df8.csv", row.names = FALSE)
  if (held_out_fold == 9) write.csv(test.prob.df, "IsBadBuy-oos-prob.df9.csv", row.names = FALSE)
  if (held_out_fold == 10) write.csv(test.prob.df, "IsBadBuy-oos-prob.df10.csv", row.names = FALSE)
}
