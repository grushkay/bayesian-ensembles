# ------------------------------------------------------------
# This is Carvana File 2.  Run this file once you have already 
# obtained predictions from base models (output from File 1). 
# ------------------------------------------------------------

# --------------------------
# Install and load packages.
# --------------------------

# install.packages("cvTools")
# install.packages("numDeriv")
# install.packages("LaplacesDemon")
# install.packages("pROC")

library(cvTools)
library(numDeriv)
library(LaplacesDemon)
library(pROC)

# ---------------------
# Load the predictions.
# ---------------------

test.prob.df1 <- read.csv("IsBadBuy-oos-prob.df1.csv")
test.prob.df2 <- read.csv("IsBadBuy-oos-prob.df2.csv")
test.prob.df3 <- read.csv("IsBadBuy-oos-prob.df3.csv")
test.prob.df4 <- read.csv("IsBadBuy-oos-prob.df4.csv")
test.prob.df5 <- read.csv("IsBadBuy-oos-prob.df5.csv")
test.prob.df6 <- read.csv("IsBadBuy-oos-prob.df6.csv")
test.prob.df7 <- read.csv("IsBadBuy-oos-prob.df7.csv")
test.prob.df8 <- read.csv("IsBadBuy-oos-prob.df8.csv")
test.prob.df9 <- read.csv("IsBadBuy-oos-prob.df9.csv")
test.prob.df10 <- read.csv("IsBadBuy-oos-prob.df10.csv")
prob.df <- rbind(test.prob.df1, test.prob.df2, test.prob.df3, test.prob.df4, test.prob.df5, 
                 test.prob.df6, test.prob.df7, test.prob.df8, test.prob.df9, test.prob.df10)
(nprob <- nrow(prob.df))
colnames(prob.df) <- c("rlr", "rf", "xgb", "IsBadBuy")

# Impute negative probabilities from the random forest.  Set to lowest positive value.
summary(prob.df)
prob.df$rf[prob.df$rf <= 0] <- min(prob.df$rf[prob.df$rf > 0])
prob.df$rf[prob.df$rf >= 1] <- min(prob.df$rf[prob.df$rf < 1])
summary(prob.df)

# -------------------------
# Set up the scoring rules.
# -------------------------

log.loss <- function(pred, real) {
  return(-mean(real * log(pred) + (1 - real) * log(1 - pred)))
}

asym.score <- function(pred, real, naive) {
  TT <- ifelse(pred <= naive,log(1)-log(1-naive), log(1)-log(naive))
  S <- ifelse(real == 1, (log(pred) - log(naive)) / TT, (log(1-pred) - log(1-naive)) / TT)
  return(mean(S))
}

# The two functions below come from https://www.kaggle.com/c/DontGetKicked/discussion/937.
# The function Gini below is the "Gini index" Kaggle used on its leaderboard.
# Despite the names of these two functions, normalizedGini = 2 * auc - 1 where auc is the area under the curve (AUC) according to the pROC package in R.
# To convert a Kaggle "Gini index" to an auc score, you divide the Kaggle "Gini index" by Gini(Actuals, Actuals), then add 1 and divide the resulting quantity by 2. In the training set, Gini(Actuals, Actuals) = 0.4385062, which a competitor can verify on his/her own.  In the testing set used on Kaggle's leaderboard, Jeff Moser (a developer at Kaggle) reports (in a discussion post at the link above) that Gini(Actuals, Actuals) = 0.43639.  Thus, for a Kaggle "Gini index" on the leaderboard, the corresponding AUC is given by
#
# AUC = (Kaggle Leaderboard "Gini index" / 0.43639 + 1) / 2.    
#

# Leaderboard score.
Gini <- function(a, p) {
  if (length(a) !=  length(p)) stop("Actual and Predicted need to be equal lengths!")
  temp.df <- data.frame(actual = a, pred = p, range=c(1:length(a)))
  temp.df <- temp.df[order(-temp.df$pred, temp.df$range),]
  population.delta <- 1 / length(a)
  total.losses <- sum(a)
  null.losses <- rep(population.delta, length(a))
  accum.losses <- temp.df$actual / total.losses
  gini.sum <- cumsum(accum.losses - null.losses)
  sum(gini.sum) / length(a)
}

# normalizedGini = 2 * auc - 1.
normalizedGini <- function(aa, pp) {
  Gini(aa,pp) / Gini(aa,aa)
}

# ---------------------------
# Make our own link function.
# ---------------------------

vlog <- function() {
  ## link
  linkfun <- function(y) qpe(y, sigma=s, kappa = power)
  ## inverse link
  linkinv <- function(eta)  ppe(eta, sigma=s, kappa = power)
  ## derivative of invlink wrt eta
  mu.eta <- function(eta) { dpe(eta, sigma=s, kappa = power) }
  valideta <- function(eta) TRUE
  link <- "qnormp"
  structure(list(linkfun = linkfun, linkinv = linkinv,
                 mu.eta = mu.eta, valideta = valideta, 
                 name = link),
            class = "link-glm")
}

# Set up the quantile function of the normal exponential-power (nep) distribution.
set.seed(201)
ntrials <- 100000
sample_unif_1 <- runif(ntrials)
sample_unif_2 <- runif(ntrials)
s <- 1

qnep <- function(prob_vec, power, v_i) {
  z_0 <- qpe(sample_unif_1, sigma = s, kappa = power)  # p is the power parameter.
  x_0 <- qnorm(sample_unif_2)
  return(quantile(z_0 + sqrt(v_i) * x_0, prob_vec))
}

pnep <- function(x, power, v_i) {
  z_0 <- qpe(sample_unif_1, sigma = s, kappa = power)  # p is the power parameter.
  x_0 <- qnorm(sample_unif_2)
  cdf <- ecdf(z_0 + sqrt(v_i) * x_0)
  return(cdf(x))
}

# --------------------------------------------
# Initialization Steps: Regressions for v_i's.
# --------------------------------------------

# Initialize loop.
initial_power <- 2
final_power <- 6
increment <- 0.25
nPowers <- (final_power - initial_power) / increment + 1
v_1.df <- data.frame("power" = seq(initial_power, final_power, increment), 
                     "Fold_1" =  rep(NA, nPowers),
                     "Fold_2" =  rep(NA, nPowers),
                     "Fold_3" =  rep(NA, nPowers),
                     "Fold_4" =  rep(NA, nPowers),
                     "Fold_5" =  rep(NA, nPowers),
                     "Fold_6" =  rep(NA, nPowers),
                     "Fold_7" =  rep(NA, nPowers),
                     "Fold_8" =  rep(NA, nPowers),
                     "Fold_9" =  rep(NA, nPowers),
                     "Fold_10" =  rep(NA, nPowers))
v_2.df <- v_1.df
v_3.df <- v_1.df
p_0.df <- data.frame("power" = seq(initial_power, final_power, increment), 
                     "p_0" =  rep(NA, nPowers))

nfolds <- 10
set.seed(201)
folds <- cvFolds(nprob, K = nfolds)

# This loop takes 3.4 minutes to run.
ptm <- proc.time()
for (q in 1:nfolds) {
  power <- 2
  vv <- vlog()
  v_1 <- 0
  v_2 <- 0
  v_3 <- 0
  for (i in 1:nPowers) {
    info.df <- data.frame("rlr" = qnep(prob.df$rlr, power, v_1), 
                          "rf" =  qnep(prob.df$rf, power, v_2), 
                          "xgb" =  qnep(prob.df$xgb, power, v_3), 
                          "IsBadBuy" = prob.df$IsBadBuy)
    info.train.df <- info.df[folds$subsets[folds$which != q], ] 
    glm.all <- glm(IsBadBuy ~ rlr + rf + xgb, data = info.train.df, 
                   family = binomial(link = vv))
    summary(glm.all)
    prop_reg.df <- data.frame("rlr" = glm.all$coefficients[2] * info.train.df$rlr,
                              "rf" = glm.all$coefficients[3] * info.train.df$rf, 
                              "xgb" = glm.all$coefficients[4] * info.train.df$xgb)
    prop_reg.df[ , "y_12"] <- prop_reg.df[1] + prop_reg.df[2]
    prop_reg.df[ , "y_13"] <- prop_reg.df[1] + prop_reg.df[3]
    prop_reg.df[ , "y_23"] <- prop_reg.df[2] + prop_reg.df[3]
    lm_12 <- lm(y_12 ~ xgb, data = prop_reg.df)
    lm_13 <- lm(y_13 ~ rf, data = prop_reg.df)
    lm_23 <- lm(y_23 ~ rlr, data = prop_reg.df)
    v_0 <- var(prop_reg.df[1] + prop_reg.df[2] + prop_reg.df[3])
    v_1 <- summary(lm_23)$sigma^2
    v_2 <- summary(lm_13)$sigma^2
    v_3 <- summary(lm_12)$sigma^2
    v_1.df[i, 1 + q] <- v_1
    v_2.df[i, 1 + q] <- v_2
    v_3.df[i, 1 + q] <- v_3
    beta_1 <- glm.all$coefficients[2] / ifelse(power == 2, sqrt(1 + v_1), 1)
    beta_2 <- glm.all$coefficients[3] / ifelse(power == 2, sqrt(1 + v_2), 1)
    beta_3 <- glm.all$coefficients[4] / ifelse(power == 2, sqrt(1 + v_3), 1)
    beta_0 <- 1 - beta_1 - beta_2 - beta_3
    p_0.df[i, 2] <- pnep(glm.all$coefficients[1] / beta_0, power, v_0) 
    power <- power + increment
    vv <- vlog()
  }
}
proc.time() - ptm

write.csv(v_1.df, "Carvana_v_1_by_fold.csv", row.names = FALSE)
write.csv(v_2.df, "Carvana_v_2_by_fold.csv", row.names = FALSE)
write.csv(v_3.df, "Carvana_v_3_by_fold.csv", row.names = FALSE)


# ------------------------------
# Stacker with cross-validation.
# ------------------------------

nfolds <- 10
set.seed(201)
folds <- cvFolds(nprob, K = nfolds)
loss.df <- data.frame("rlr" = rep(NA, nfolds),
                      "rf" = rep(NA, nfolds),
                      "xgb" = rep(NA, nfolds),
                      "p_bar" = rep(NA, nfolds),
                      "p_Prob_no_int" = rep(NA, nfolds),
                      "p_hat" = rep(NA, nfolds))

asym.df <- data.frame("rlr" = rep(NA, nfolds),
                      "rf" = rep(NA, nfolds),
                      "xgb" = rep(NA, nfolds),
                      "p_bar" = rep(NA, nfolds),
                      "p_Prob_no_int" = rep(NA, nfolds),
                      "p_hat" = rep(NA, nfolds))

AUC.df <- data.frame("rlr" = rep(NA, nfolds),
                     "rf" = rep(NA, nfolds),
                     "xgb" = rep(NA, nfolds),
                     "p_bar" = rep(NA, nfolds),
                     "p_Prob_no_int" = rep(NA, nfolds),
                     "p_hat" = rep(NA, nfolds))

Gini.df <- data.frame("rlr" = rep(NA, nfolds),
                     "rf" = rep(NA, nfolds),
                     "xgb" = rep(NA, nfolds),
                     "p_bar" = rep(NA, nfolds),
                     "p_Prob_no_int" = rep(NA, nfolds),
                     "p_hat" = rep(NA, nfolds))

power <- 4.25
vv <- vlog()
v_1.df <- read.csv("Carvana_v_1_by_fold.csv")
v_2.df <- read.csv("Carvana_v_2_by_fold.csv")
v_3.df <- read.csv("Carvana_v_3_by_fold.csv")

ptm <- proc.time()
for (q in 1:nfolds) {
  
  # Fit the stacker.
  info.df <- data.frame("rlr" = qnep(prob.df$rlr, power, v_1.df[(power - 2)/increment + 1, 1 + q]), 
                        "rf" =  qnep(prob.df$rf, power, v_2.df[(power - 2)/increment + 1, 1 + q]), 
                        "xgb" =  qnep(prob.df$xgb, power, v_3.df[(power - 2)/increment + 1, 1 + q]), 
                        "IsBadBuy" = prob.df$IsBadBuy)
  info.probit.df <- data.frame("rlr" = qnorm(prob.df$rlr), 
                               "rf" =  qnorm(prob.df$rf), 
                               "xgb" =  qnorm(prob.df$xgb), 
                               "IsBadBuy" = prob.df$IsBadBuy)
  info.train.df <- info.df[folds$subsets[folds$which != q], ] 
  info.test.df <- info.df[folds$subsets[folds$which == q], ]
  stacker.fit <- glm(IsBadBuy ~ rlr + rf + xgb, data = info.train.df, family = binomial(link = vv))
  stacker.pred <- predict(stacker.fit, info.test.df, type = "response")
  
  info.probit.train.df <- info.probit.df[folds$subsets[folds$which != q], ] 
  info.probit.test.df <- info.probit.df[folds$subsets[folds$which == q], ]
  probit_no_intercept.fit <- glm(IsBadBuy ~ rlr + rf + xgb - 1, data = info.probit.train.df, family = binomial(link = "probit"))
  probit_no_intercept.pred <- predict(probit_no_intercept.fit, info.probit.test.df, type = "response")
  
  # Score the models and the ensembles.
  prob.train.df <- prob.df[folds$subsets[folds$which != q], ] 
  prob.test.df <- prob.df[folds$subsets[folds$which == q], ]
  naive <- rep(mean(prob.train.df$IsBadBuy), nrow(prob.test.df))
  loss.df[q, 1] <- log.loss(prob.test.df$rlr, info.test.df$IsBadBuy)
  loss.df[q, 2] <- log.loss(prob.test.df$rf, info.test.df$IsBadBuy)
  loss.df[q, 3] <- log.loss(prob.test.df$xgb, info.test.df$IsBadBuy)
  loss.df[q, 4] <- log.loss((prob.test.df$rlr + prob.test.df$rf + prob.test.df$xgb)/3, info.test.df$IsBadBuy)
  loss.df[q, 5] <- log.loss(probit_no_intercept.pred, info.test.df$IsBadBuy)
  loss.df[q, 6] <- log.loss(stacker.pred, info.test.df$IsBadBuy)
  asym.df[q, 1] <- asym.score(prob.test.df$rlr, info.test.df$IsBadBuy, naive)
  asym.df[q, 2] <- asym.score(prob.test.df$rf, info.test.df$IsBadBuy, naive)
  asym.df[q, 3] <- asym.score(prob.test.df$xgb, info.test.df$IsBadBuy, naive)
  asym.df[q, 4] <- asym.score((prob.test.df$rlr + prob.test.df$rf + prob.test.df$xgb)/3, info.test.df$IsBadBuy, naive)
  asym.df[q, 5] <- asym.score(probit_no_intercept.pred, info.test.df$IsBadBuy, naive)
  asym.df[q, 6] <- asym.score(stacker.pred, info.test.df$IsBadBuy, naive)
  AUC.df[q, 1] <- auc(info.test.df$IsBadBuy, prob.test.df$rlr)
  AUC.df[q, 2] <- auc(info.test.df$IsBadBuy,prob.test.df$rf)
  AUC.df[q, 3] <- auc(info.test.df$IsBadBuy,prob.test.df$xgb)
  AUC.df[q, 4] <- auc(info.test.df$IsBadBuy,(prob.test.df$rlr + prob.test.df$rf + prob.test.df$xgb)/3)
  AUC.df[q, 5] <- auc(info.test.df$IsBadBuy, probit_no_intercept.pred)
  AUC.df[q, 6] <- auc(info.test.df$IsBadBuy, stacker.pred)
  Gini.df[q, 1] <- Gini(info.test.df$IsBadBuy, prob.test.df$rlr)
  Gini.df[q, 2] <- Gini(info.test.df$IsBadBuy, prob.test.df$rf)
  Gini.df[q, 3] <- Gini(info.test.df$IsBadBuy, prob.test.df$xgb)
  Gini.df[q, 4] <- Gini(info.test.df$IsBadBuy, (prob.test.df$rlr + prob.test.df$rf + prob.test.df$xgb)/3)
  Gini.df[q, 5] <- Gini(info.test.df$IsBadBuy, probit_no_intercept.pred)
  Gini.df[q, 6] <- Gini(info.test.df$IsBadBuy, stacker.pred)
}
proc.time() - ptm

# Check the scores of the different forecasts.
loss.df
colMeans(loss.df)
asym.df
colMeans(asym.df)
AUC.df
colMeans(AUC.df)
Gini.df
colMeans(Gini.df)

  
# -----------------------------------------------------------------
# Regressions for v_i's for final model. Run on all data, no folds.   
# -----------------------------------------------------------------

initial_power <- 2
final_power <- 4.50
increment <- 0.25
nPowers <- (final_power - initial_power) / increment + 1
v_0.df <- data.frame("power" = seq(initial_power, final_power, increment), 
                     "v_i" =  rep(NA, nPowers))
v_1.df <- data.frame("power" = seq(initial_power, final_power, increment), 
                     "v_i" =  rep(NA, nPowers))
v_2.df <- v_1.df
v_3.df <- v_1.df
p_0.df <- data.frame("power" = seq(initial_power, final_power, increment), 
                     "p_0" =  rep(NA, nPowers))

power <- 2
vv <- vlog()
v_1 <- 0
v_2 <- 0
v_3 <- 0

for (i in 1:nPowers) {
  info.df <- data.frame("rlr" = qnep(prob.df$rlr, power, v_1), 
                        "rf" =  qnep(prob.df$rf, power, v_2), 
                        "xgb" =  qnep(prob.df$xgb, power, v_3), 
                        "IsBadBuy" = prob.df$IsBadBuy)
  glm.all <- glm(IsBadBuy ~ rlr + rf + xgb, data = info.df, 
                 family = binomial(link = vv))
  summary(glm.all)
  prop_reg.df <- data.frame("rlr" = glm.all$coefficients[2] * info.df$rlr,
                            "rf" = glm.all$coefficients[3] * info.df$rf, 
                            "xgb" = glm.all$coefficients[4] * info.df$xgb)
  prop_reg.df[ , "y_12"] <- prop_reg.df[1] + prop_reg.df[2]
  prop_reg.df[ , "y_13"] <- prop_reg.df[1] + prop_reg.df[3]
  prop_reg.df[ , "y_23"] <- prop_reg.df[2] + prop_reg.df[3]
  lm_12 <- lm(y_12 ~ xgb, data = prop_reg.df)
  lm_13 <- lm(y_13 ~ rf, data = prop_reg.df)
  lm_23 <- lm(y_23 ~ rlr, data = prop_reg.df)
  v_0 <- var(prop_reg.df[1] + prop_reg.df[2] + prop_reg.df[3])
  v_1 <- summary(lm_23)$sigma^2
  v_2 <- summary(lm_13)$sigma^2
  v_3 <- summary(lm_12)$sigma^2
  v_0.df[i, 2] <- v_0
  v_1.df[i, 2] <- v_1
  v_2.df[i, 2] <- v_2
  v_3.df[i, 2] <- v_3
  beta_1 <- glm.all$coefficients[2] / ifelse(power == 2, sqrt(1 + v_1), 1)
  beta_2 <- glm.all$coefficients[3] / ifelse(power == 2, sqrt(1 + v_2), 1)
  beta_3 <- glm.all$coefficients[4] / ifelse(power == 2, sqrt(1 + v_3), 1)
  beta_0 <- 1 - beta_1 - beta_2 - beta_3
  p_0.df[i, 2] <- pnep(glm.all$coefficients[1] / beta_0, power, v_0) 
  power <- power + increment
}

power <- 4.25
vv <- vlog()
(v_0 <- v_0.df[(power - 2)/increment + 1, 2])
(v_1 <- v_1.df[(power - 2)/increment + 1, 2]) 
(v_2 <- v_2.df[(power - 2)/increment + 1, 2]) 
(v_3 <- v_3.df[(power - 2)/increment + 1, 2])
info.df <- data.frame("rlr" = qnep(prob.df$rlr, power, v_1), 
                      "rf" =  qnep(prob.df$rf, power, v_2), 
                      "xgb" =  qnep(prob.df$xgb, power, v_3), 
                      "IsBadBuy" = prob.df$IsBadBuy)
glm.all <- glm(IsBadBuy ~ rlr + rf + xgb, data = info.df, 
               family = binomial(link = vv))
summary(glm.all)
beta_1 <- glm.all$coefficients[2] / ifelse(power == 2, sqrt(1 + v_1), 1)
beta_2 <- glm.all$coefficients[3] / ifelse(power == 2, sqrt(1 + v_2), 1)
beta_3 <- glm.all$coefficients[4] / ifelse(power == 2, sqrt(1 + v_3), 1)
(beta_0 <- 1 - beta_1 - beta_2 - beta_3)
pnep(glm.all$coefficients[1] / beta_0, power, v_0)

# Extremizing
stacker.pred <- predict(glm.all, info.df, type = "response")
prob.final.df <- data.frame("rlr" = prob.df$rlr, 
                            "rf" =  prob.df$rf, 
                            "xgb" =  prob.df$xgb, 
                            "p_bar" = (prob.df$rlr + prob.df$rf + prob.df$xgb)/3,
                            "p_hat" = stacker.pred,
                            "IsBadBuy" = prob.df$IsBadBuy)

summary(prob.final.df)
base_rate <- mean(prob.final.df$IsBadBuy)
prob.final.df$extremize0 <- ifelse(prob.final.df$p_hat < prob.final.df$p_bar & prob.final.df$p_bar < base_rate, 1, 0)
prob.final.df$extremize1 <- ifelse(prob.final.df$p_hat > prob.final.df$p_bar & prob.final.df$p_bar > base_rate, 1, 0)
mean(prob.final.df$extremize0) 
mean(prob.final.df$extremize1) 
mean(prob.final.df$extremize0) + mean(prob.final.df$extremize1)

