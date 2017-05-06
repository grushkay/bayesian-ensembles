# ------------------------------------------------------
# This is Carvana File 3.  Run this file after File 2, 
# using parameters estimated for the final ensemble.  
# -----------------------------------------------------

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
library(LaplacesDemon)

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

train.df <- read.csv("training.csv", na.strings = c("", "NULL"), stringsAsFactors = FALSE)
ntrain <- dim(train.df)[1]
test.df <- read.csv("test.csv", na.strings = c("", "NULL"), stringsAsFactors = FALSE)
ntest <- dim(test.df)[1]
two.new.test.columns.df <- data.frame("RefId" = test.df$RefId, 
                             "IsBadBuy" = rep(NA, dim(test.df)[1]))
append.test.df <- cbind(two.new.test.columns.df, test.df[, 2:dim(test.df)[2]])
all.df <- rbind(train.df, append.test.df)


# ---------------
# Clean the data.
# ---------------

# Manual transmissions appears as either "Manual" or "MANUAL".  Convert all to "Manual".
all.df$Transmission[all.df$Transmission == "Manual"] <- "MANUAL"
all.df$Transmission <- as.factor(as.character(all.df$Transmission))
summary(all.df$Transmission)

# Add variables for week, month, and year.
all.df$PurchDate <- as.Date(all.df$PurchDate, format = "%m/%d/%Y")
all.df$PurchWeek <- week(all.df$PurchDate)
all.df$PurchMonth <- month(all.df$PurchDate)
all.df$PurchYear <- year(all.df$PurchDate)

# Convert some variables into factors.
factor.variables <- c(4, 7:14, 16:18, 27:31, 33, 35:37)
for(i in factor.variables) {
  all.df[, i] <- factor(all.df[, i], exclude = NULL)
}

ImputeData <- function(vec, mn) {
  ifelse(is.na(vec), mn, vec)
}

# Data imputation.
data.mean <- sapply(all.df[ , 19:26], mean, na.rm = TRUE)

for(i in 19:26) {
  all.df[, i] <- ImputeData(all.df[ , i], data.mean[i - 18])
}

# Examine the results.
all.df$Set <- c(rep("Train", ntrain), rep("Test", ntest))  # Add a training/testing set indicator.
all.summary.df <- data.frame("Variable" = seq(1, dim(all.df)[2]), 
                             "Class" = sapply(all.df, class), 
                             "NA Count" = colSums(sapply(all.df, is.na)), 
                             "nLevels" = sapply(all.df, nlevels))


# ---------------------------
# Partition and process data.
# ---------------------------

selected.variables <- c(4:37)

# For lasso.
x.lasso <- sparse.model.matrix(~ 0 + ., data = all.df[, selected.variables])
xtrain.lasso <- x.lasso[all.df$Set == "Train", ]
xtest.lasso <- x.lasso[all.df$Set == "Test", ]
ytrain.lasso <- as.vector(all.df[all.df$Set == "Train", 2])

# For random forest. Convert the factors with levels > 32 to numeric.
all.rf.df <- all.df[, selected.variables]
for(i in c(4:7, 26:28, 32)) {
  levels(all.rf.df[, i]) <- seq(1, length(levels(all.rf.df[, i])))
  all.rf.df[, i] <- as.numeric(levels(all.rf.df[, i]))[all.rf.df[, i]]
}
# Convert NA to "NA" for random forest.
for(i in c(8:11, 13:15, 24, 25, 30)) {
  levels(all.rf.df[, i])[is.na(levels(all.rf.df[, i]))] <- "NA"
}

xtrain.rf <- all.rf.df[all.df$Set == "Train", ]
xtest.rf <- all.rf.df[all.df$Set == "Test", ]
ytrain.rf <- as.vector(all.df[all.df$Set == "Train", 2])

# For xgboost.
x.xgb <- sparse.model.matrix(~ 0 + ., data = all.df[, selected.variables])
xtrain.xgb <- x.xgb[all.df$Set == "Train", ]
xtest.xgb <- x.xgb[all.df$Set == "Test", ]
ytrain.xgb <- as.vector(all.df[all.df$Set == "Train", 2])


# --------------------------------------
# Run a regularized logistic regression.
# --------------------------------------

# This run takes about 30 minutes.
ptm <- proc.time()
set.seed(201)
lasso.model.cv <- cv.glmnet(xtrain.lasso, ytrain.lasso, family = "binomial", parallel = TRUE)
proc.time() - ptm
lasso.model.cv.pred <- predict(lasso.model.cv, newx = xtest.lasso, s = "lambda.min", type = "response") 


# --------------------
# Run a random forest.
# --------------------

# This run takes less than 10 minutes.
ptm <- proc.time()
rf <- foreach(j = 1:workers, .combine=combine, .multicombine=TRUE, .inorder=FALSE, .packages = c("randomForest")) %dopar% {
  set.seed(j)
  randomForest(xtrain.rf, ytrain.rf, sampsize = 20000, ntree = ceiling(504/workers))
}
proc.time() - ptm
test.prob.rf <- predict(rf, xtest.rf)


# -----------------------------------------
# Run extreme gradient boosted trees model.  
# -----------------------------------------

# The two runs below take less than 5 minutes.

# Cross-validation for xgboost.
cv_for_xgb_on <- 1
if (cv_for_xgb_on) {
  param <- list(objective = "binary:logistic")
  set.seed(201)
  ptm <- proc.time()
  xgb.trees.cv <- xgb.cv(xtrain.xgb, ytrain.xgb, params = param, nthread = workers, 
                         nfold = 10, nrounds = 200)
  proc.time() - ptm
  plot(xgb.trees.cv$evaluation_log$train_error_mean, ylim=c(0.09,0.12))
  lines(xgb.trees.cv$evaluation_log$test_error_mean)
  min(xgb.trees.cv$evaluation_log$test_error_mean)
  nrounds.cv = which(xgb.trees.cv$evaluation_log$test_error_mean == min(xgb.trees.cv$evaluation_log$test_error_mean))
}

# Fit the best xgboost model.
set.seed(201)
ptm <- proc.time()
xgb.trees <- xgboost(xtrain.xgb, ytrain.xgb, nround = nrounds.cv, objective = "binary:logistic")
proc.time() - ptm 
test.prob.xgb <- predict(xgb.trees, xtest.xgb)


# -----------------------------------------------------
# Write out predictions, models, and data for test set.  
# -----------------------------------------------------

test.prob.rlr <- lasso.model.cv.pred
test.prob.rf <- test.prob.rf 
test.prob.xgb <- test.prob.xgb
test.prob.df <- data.frame("rlrprob" = as.numeric(test.prob.rlr), "rfprob" = test.prob.rf, "xgbprob" = test.prob.xgb)
write.csv(test.prob.df, "IsBadBuy-test-prob-kaggle.df.csv", row.names = FALSE)


# ------------------------------------------------
# Create Kaggle submission of xgboost predictions.
# ------------------------------------------------

submission.xgb.df <- append.test.df[, 1:2]
submission.xgb.df[2] <- test.prob.df$xgb
head(submission.xgb.df)
write.csv(submission.xgb.df, "Kaggle-submission-xgboost.csv", row.names = FALSE)


# -------------------------------------------------------------------
# Load out-of-sample base model predictions for training the stacker.
# -------------------------------------------------------------------

# Predictions.
prob.df1 <- read.csv("IsBadBuy-oos-prob.df1.csv")
prob.df2 <- read.csv("IsBadBuy-oos-prob.df2.csv")
prob.df3 <- read.csv("IsBadBuy-oos-prob.df3.csv")
prob.df4 <- read.csv("IsBadBuy-oos-prob.df4.csv")
prob.df5 <- read.csv("IsBadBuy-oos-prob.df5.csv")
prob.df6 <- read.csv("IsBadBuy-oos-prob.df6.csv")
prob.df7 <- read.csv("IsBadBuy-oos-prob.df7.csv")
prob.df8 <- read.csv("IsBadBuy-oos-prob.df8.csv")
prob.df9 <- read.csv("IsBadBuy-oos-prob.df9.csv")
prob.df10 <- read.csv("IsBadBuy-oos-prob.df10.csv")
train.prob.df <- rbind(prob.df1, prob.df2, prob.df3, prob.df4, prob.df5, 
                 prob.df6, prob.df7, prob.df8, prob.df9, prob.df10)
colnames(train.prob.df) <- c("rlr", "rf", "xgb", "IsBadBuy")

# Impute negative probabilities from the random forest.  Set to lowest positive value.
train.prob.df$rf[train.prob.df$rf < 0] <- min(train.prob.df$rf[train.prob.df$rf >= 0])

# Predictions on test set.
test.prob.df <- read.csv("IsBadBuy-test-prob-kaggle.df.csv")
summary(test.prob.df)
colnames(test.prob.df) <- c ("rlr", "rf", "xgb")
# Impute negative probabilities from the random forest.  Set to lowest positive value.
test.prob.df$rf[test.prob.df$rf < 0] <- min(test.prob.df$rf[test.prob.df$rf >= 0])


# ---------------------------------------
# Train the stacker and make predictions.
# ---------------------------------------

set.seed(201)
ntrials <- 100000
sample_unif_1 <- runif(ntrials)
sample_unif_2 <- runif(ntrials)

qnep <- function(prob_vec, power, v_i) {
  z_0 <- qpe(sample_unif_1, kappa=  power)  # p is the power parameter.
  x_0 <- qnorm(sample_unif_2)
  return(quantile(z_0 + sqrt(v_i) * x_0, prob_vec))
}

# Make our own link function.
vlog <- function() {
  ## link
  linkfun <- function(y) qpe(y, kappa = power)
  ## inverse link
  linkinv <- function(eta)  ppe(eta, kappa = power)
  ## derivative of invlink wrt eta
  mu.eta <- function(eta) { dpe(eta, kappa = power) }
  valideta <- function(eta) TRUE
  link <- "qpe"
  structure(list(linkfun = linkfun, linkinv = linkinv,
                 mu.eta = mu.eta, valideta = valideta, 
                 name = link),
            class = "link-glm")
}

# With the best power parameter, run the exponential-power ensemble on all of the training data.
power <- 4.25
v_1 <- 0.05099972
v_2 <-  0.01887154
v_3 <- 0.00560271
vv <- vlog()
info.df <- data.frame("rlr" = qnep(train.prob.df$rlr, power, v_1), 
                      "rf" =  qnep(train.prob.df$rf, power, v_2), 
                      "xgb" =  qnep(train.prob.df$xgb, power, v_3), 
                      "IsBadBuy" = train.prob.df$IsBadBuy)
glm.our_link <- glm(IsBadBuy ~ rlr + rf + xgb, data = info.df, 
               family = binomial(link = vv))
summary(glm.our_link)
info.test.df <- data.frame("rlr" = qnep(test.prob.df$rlr, power, v_1), 
                           "rf" =  qnep(test.prob.df$rf, power, v_2), 
                           "xgb" =  qnep(test.prob.df$xgb, power, v_3))
glm.our_link.pred <- predict(glm.our_link, info.test.df, type = "response")


# -------------------------------------------------------
# Create Kaggle submission of the ensemble's predictions.
# -------------------------------------------------------

submission.EP.df <- append.test.df[, 1:2]
submission.EP.df[2] <- glm.our_link.pred
head(submission.EP.df)
write.csv(submission.EP.df, "Kaggle-submission-EPE.csv", row.names = FALSE)
