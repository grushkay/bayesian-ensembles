# -----------------------------------------------------------------------------------
# This is Fannie-2007 File 1.  Run this first to obtain predictions from base models. 
# -----------------------------------------------------------------------------------

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

rawSQLQ1.df <- read.csv("acquisitions_with_default_infoQ1.csv", header = TRUE, sep = '|', na.strings=c("", "NULL"))
rawSQLQ2.df <- read.csv("acquisitions_with_default_infoQ2.csv", header = TRUE, sep = '|', na.strings=c("", "NULL"))
rawSQLQ3.df <- read.csv("acquisitions_with_default_infoQ3.csv", header = TRUE, sep = '|', na.strings=c("", "NULL"))
rawSQLQ4.df <- read.csv("acquisitions_with_default_infoQ4.csv", header = TRUE, sep = '|', na.strings=c("", "NULL"))
raw.df <- rbind(rawSQLQ1.df, rawSQLQ2.df, rawSQLQ3.df, rawSQLQ4.df)

# Create dependent variable: default/no default.
raw.df$DEFAULT_FLAG <- ifelse(raw.df$ZERO_BALANCE_CODE == 3 | raw.df$ZERO_BALANCE_CODE == 9, 1, 0)
raw.df$DEFAULT_FLAG <- ifelse(is.na(raw.df$DEFAULT_FLAG), 0, raw.df$DEFAULT_FLAG)
mean(na.omit(raw.df$DEFAULT_FLAG))

# Examine columns of data frame.
raw.summary.df <- data.frame("Variable" = seq(1, dim(raw.df)[2]), 
                             "Class" = sapply(raw.df, class), 
                             "NA Count" = colSums(sapply(raw.df, is.na)), 
                             "nLevels" = sapply(raw.df, nlevels))


# -------------------------------------------
# Engineer new features and process the data.
# -------------------------------------------

# Drop LAST_LOAN_AGE.
raw.df <- raw.df[ ,-27]

# Engineer DAYS_TO_FIRST_PAYMENT.
raw.df$ORIGINATION_DATE <- as.Date(paste("01/", raw.df$ORIGINATION_DATE, sep = ""), format = "%d/%m/%Y")
raw.df$FIRST_PAYMENT_DATE <- as.Date(paste("01/", raw.df$FIRST_PAYMENT_DATE, sep = ""), format = "%d/%m/%Y")
raw.df$DAYS_TO_FIRST_PAYMENT <- as.numeric(difftime(raw.df$FIRST_PAYMENT_DATE, raw.df$ORIGINATION_DATE, units = "days"))

# Engineer CO_BORROWER_CREDIT.
class(raw.df$CO_BORROWER_CREDIT_SCORE)
unique(raw.df$CO_BORROWER_CREDIT_SCORE)
hist(raw.df$CO_BORROWER_CREDIT_SCORE)
min(raw.df$CO_BORROWER_CREDIT_SCORE, na.rm = TRUE)
max(raw.df$CO_BORROWER_CREDIT_SCORE, na.rm = TRUE)
# Poor, Subprime, Acceptable, Good, Excellent
new_var_1 <- cut(raw.df$CO_BORROWER_CREDIT_SCORE, breaks = c(300, 550, 620, 680, 740, 850))
table(new_var_1)
raw.df$CO_BORROWER_CREDIT <- factor(new_var_1, levels = c("(300,550]", "(550,620]", "(620,680]", "(680,740]", "(740,850]", NA), labels = c("Poor", "Subprime", "Acceptable", "Good", "Excellent", "NA"), exclude = NULL)
table(raw.df$CO_BORROWER_CREDIT)

# Make factors of MORTGAGE_INSURANCE_PERCENTAGE and MORTGAGE_INSURANCE_TYPE.
class(raw.df$MORTGAGE_INSURANCE_PERCENTAGE)
table(raw.df$MORTGAGE_INSURANCE_PERCENTAGE)
new_var_2 <- cut(raw.df$MORTGAGE_INSURANCE_PERCENTAGE, breaks = c(1, 10, 20, 25, 30, 40))
table(new_var_2)
raw.df$MORTGAGE_INSURANCE_PERCENTAGE <- factor(new_var_2, exclude = NULL)
levels(raw.df$MORTGAGE_INSURANCE_PERCENTAGE)[is.na(levels(raw.df$MORTGAGE_INSURANCE_PERCENTAGE))] <- "NA"
table(raw.df$MORTGAGE_INSURANCE_PERCENTAGE)
raw.df$MORTGAGE_INSURANCE_TYPE <- factor(raw.df$MORTGAGE_INSURANCE_TYPE, exclude = NULL)
levels(raw.df$MORTGAGE_INSURANCE_TYPE)[is.na(levels(raw.df$MORTGAGE_INSURANCE_TYPE))] <- "NA"
table(raw.df$MORTGAGE_INSURANCE_TYPE)

# Convert ZIP_3_DIGIT to a factor.
summary(raw.df$ZIP_3_DIGIT)
raw.df$ZIP_3_DIGIT <- as.factor(raw.df$ZIP_3_DIGIT)

# Impute missing values.
clean.df <- raw.df
imputed.columns <- c(4, 9:13)  # Columns in clean.df to impute.
impute_data <- function(vec, mn) ifelse(is.na(vec), mn, vec)
for(i in imputed.columns) {
  clean.df[, i] <- impute_data(clean.df[, i], mean(clean.df[, i], na.rm=TRUE))
}

selected.columns <- c(2:6, 9:18, 20:21, 24, 28:29)  
nselected <- length(selected.columns)
dependent.column <- 27
train.df <- clean.df[, c(selected.columns, dependent.column)]  

check_selected_columns <- data.frame("Variable" = seq(1, dim(train.df)[2]), 
                                                       "Class" = sapply(train.df, class), 
                                                       "NA Count" = colSums(sapply(train.df, is.na)), 
                                                       "nLevels" = sapply(train.df, nlevels))

# For the lasso.
xtrain.lasso <- sparse.model.matrix(~ 0 + ., data = train.df[, seq(1, nselected)])
ytrain.lasso <- as.vector(train.df[, nselected + 1])

# Change ZIP_3_DIGIT to numeric for the two tree models.
train.df$ZIP_3_DIGIT <- as.numeric(as.character(train.df$ZIP_3_DIGIT))

# For the random forest.
xtrain.rf <- train.df[, seq(1, nselected)]
ytrain.rf <- as.vector(train.df[, nselected + 1])

# For the xgboost.
xtrain.xgb <- sparse.model.matrix(~ 0 + ., data = train.df[, seq(1, nselected)])
ytrain.xgb <- as.vector(train.df[, nselected + 1])


# --------------------
# Set up scoring rule.
# --------------------

log.loss <- function(pred, real) {
  return(-mean(real * log(pred) + (1 - real) * log(1 - pred)))
}


# ----------------------------------------------------
# Make out-of-sample predictions with the base models.
# ----------------------------------------------------

ntotal <- nrow(train.df)
nfolds <- 10
set.seed(201)
folds <- cvFolds(ntotal, K = nfolds)

# This loop takes 5.44 hours to run.
ptm_outer <- proc.time()
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
                             "DEFAULT_FLAG" = yvalid.xgb)
  if (held_out_fold == 1) write.csv(test.prob.df, "Fannie-2007-oos-prob.df1.csv", row.names = FALSE)
  if (held_out_fold == 2) write.csv(test.prob.df, "Fannie-2007-oos-prob.df2.csv", row.names = FALSE) 
  if (held_out_fold == 3) write.csv(test.prob.df, "Fannie-2007-oos-prob.df3.csv", row.names = FALSE)
  if (held_out_fold == 4) write.csv(test.prob.df, "Fannie-2007-oos-prob.df4.csv", row.names = FALSE)
  if (held_out_fold == 5) write.csv(test.prob.df, "Fannie-2007-oos-prob.df5.csv", row.names = FALSE)
  if (held_out_fold == 6) write.csv(test.prob.df, "Fannie-2007-oos-prob.df6.csv", row.names = FALSE)
  if (held_out_fold == 7) write.csv(test.prob.df, "Fannie-2007-oos-prob.df7.csv", row.names = FALSE)
  if (held_out_fold == 8) write.csv(test.prob.df, "Fannie-2007-oos-prob.df8.csv", row.names = FALSE)
  if (held_out_fold == 9) write.csv(test.prob.df, "Fannie-2007-oos-prob.df9.csv", row.names = FALSE)
  if (held_out_fold == 10) write.csv(test.prob.df, "Fannie-2007-oos-prob.df10.csv", row.names = FALSE)
}
proc.time() - ptm_outer





