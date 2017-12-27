# rm(list=ls())
options(scipen = 99999999)

library(fasttime)
library(favorita)
library(data.table)
library(glmnet)
library(xgboost)
fv <- fv_full <- load_fv(file = '../favorita_grocery_sales_forecasting/data/fv.rds')
# fv$train$date <- fastPOSIXct(fv$train$date)

# Sample
transform_scale <- function(x){
  x[x<0] <- 0
  log(x+1)
}
y_non_trans <- ifelse(fv$train$unit_sales<0, 1, fv$train$unit_sales + 1)
y <- transform_scale(fv$train$unit_sales) # as defined in the loss function
X <- as.data.frame(fv$train)[!names(fv$train) %in% c("id","unit_sales")]
# X <- fv$train[, !c("id","unit_sales"), with = FALSE]
head(X)

# y2 <- cut(fv$train$unit_sales, seq(floor(min(fv$train$unit_sales)), ceiling(max(fv$train$unit_sales)), by = 1 ), ordered_result = TRUE )
# y3 <- table(y2)
# barplot(y3)
# barplot
# hist(y, breaks = 100, col = 8)

set.seed(65846651)
use <- sample(1:nrow(X),100000)
X <- X[use,]
y <- y[use]
y_non_trans <- y_non_trans[use]
head(X)

## add data from other sources
# holidays_events
name <- "holidays_events"
S <- sapply(fv_full[[name]], function(x){
  x[match(X$date, fv_full[[name]]$date )]
})
colnames(S) <- names(fv_full[[name]])
X <- cbind(X,S[,colnames(S) != "date"])

# items
name <- "items"
S <- sapply(fv_full[[name]], function(x){
  x[match(X$item_nbr, fv_full[[name]]$item_nbr )]
})
colnames(S) <- names(fv_full[[name]])
X <- cbind(X,S[,colnames(S) != "item_nbr"])

# stores
name <- "stores"
S <- sapply(fv_full[[name]], function(x){
  x[match(X$store_nbr, fv_full[[name]]$store_nbr )]
})
colnames(S) <- names(fv_full[[name]])
X <- cbind(X,S[,colnames(S) != "store_nbr"])

# # transactions - not done!! but do this. figure out how sales should be a feature. remember that you can not use it on day of prediction
# # use transactions as a polularity estimate 
# name <- "transactions"
# str(fv_full[[name]])
# DF_store_nbr <- split(fv_full[[name]], fv_full[[name]]$store_nbr) # split into dataframe by each store_nbr
# head(DF_store_nbr[[1]])
# plot(DF_store_nbr[[1]]$transactions[1:500], type = "b")
# 
# S <- sapply(fv_full[[name]], function(x){
#   x[match(X$store_nbr, fv_full[[name]]$store_nbr )]
# })
# colnames(S) <- names(fv_full[[name]])
# S <- as.data.frame(S, stringsAsFactors = FALSE)
# S$store_nbr <- as.numeric(S$store_nbr)
# S$transactions <- as.numeric(S$transactions)
# str(S)
# head(S)
# dim(S)
# dim(X)
# summary(S[S$store_nbr==32,])
# X <- cbind(X,S[,colnames(S) != "store_nbr"])


## make calandar information ready in data set as discussed with Søren

# You have omitted transactions. this might not be optimal!

## Fix X after merge<head(X)
summary(X)
X[sapply(X,data.class) %in% c("factor", "logical", "character")] <-
  lapply(X[sapply(X,data.class) %in% c("factor", "logical", "character")], function(x){
    factor(x, exclude = NULL)
  })
summary(X)
X$date <- as.character(X$date)

# use date as numeric but recycle for 365. e.g. modulus 365
X$date_day <- yday(fastPOSIXct(X$date))
X$date_month <- month(fastPOSIXct(X$date))
X$date_week <- week(fastPOSIXct(X$date))


# X2 <- data.frame(X)
# X2$date <- as.numeric(X$date)
# X2$store_nbr <- factor(X2$store_nbr)
# X2$item_nbr <- factor(X2$item_nbr)
# X2$onpromotion <- factor(X2$onpromotion) # remember NA
# levels(X2$onpromotion) = c(levels(X2$onpromotion),"na")
# X2$onpromotion[is.na(X2$onpromotion)] = "na"
# X2$holidays_events <- factor(X2$holidays_events) # remember NA
# levels(X2$holidays_events) = c(levels(X2$holidays_events),"na")
# X2$holidays_events[is.na(X2$holidays_events)] = "na"

# X2 <- sparse.model.matrix(~.-1, data = X2 )
# dim(X2)
str(X)
X_design <- sparse.model.matrix(~.-1 , data = X[!sapply(X,data.class) %in% c("character")], contrasts.arg = lapply( X[ sapply(X, is.factor)], contrasts, contrasts=FALSE))
dim(X_design)
head(X_design)


alpha <- 0.5
set.seed(6848684)
EN_CV <- cv.glmnet(X_design, y, intercept = TRUE, alpha = alpha, nfolds = 10 ) #, lambda = exp(seq(-2,10,0.05))
plot(EN_CV)
# y[y<0] <- 0
# EN_CV <- cv.glmnet(X_design, y, intercept = TRUE, alpha = alpha, family = "poisson")
# consider time trend
# Store number (or stores) matter
EN <- glmnet(X_design, y, intercept = TRUE, alpha = alpha)
# EN <- glmnet(X_design, y, intercept = TRUE, alpha = alpha, family = "poisson")
mean((y-mean(y))^2)
coef(EN, s = EN_CV$lambda.min)


XG_cv <- xgb.cv(data = X_design,
                label = y,
                booster = "gbtree",
                objective = "reg:linear",
                eval_metric = "rmse",
                nfold = 10,
                max.depth = 6,
                eta = 0.1,
                nround = 500,
                subsample = 0.5,
                colsample_bytree = 0.5,
                early_stopping_rounds = 100
)
set.seed(54115)
XG_model <- xgboost(data = X_design,
                label = y,
                booster = "gbtree",
                objective = "reg:linear",
                max.depth = 6,
                eta = 0.1,
                nround = 1000,
                subsample = 0.5,
                colsample_bytree = 0.5
)

# http://xgboost.readthedocs.io/en/latest/parameter.html#general-parameters
XG_cv_pois <- xgb.cv(data = X_design,
                label = y_non_trans,
                booster = "gbtree",
                objective = "count:poisson",
                eval_metric = c("rmse"),
                nfold = 10,
                max.depth = 6,
                eta = 0.1,
                nround = 500,
                subsample = 0.5,
                colsample_bytree = 0.5,
                early_stopping_rounds = 100
)
set.seed(54115)
XG_model_pois <- xgboost(data = X_design,
                    label = y_non_trans,
                    booster = "gbtree",
                    objective = "count:poisson",
                    max.depth = 6,
                    eta = 0.1,
                    nround = 1000,
                    subsample = 0.5,
                    colsample_bytree = 0.5
)
XG_model_pois_pois <- xgboost(data = X_design,
                         label = y,
                         booster = "gbtree",
                         objective = "count:poisson",
                         max.depth = 6,
                         eta = 0.1,
                         nround = 1000,
                         subsample = 0.5,
                         colsample_bytree = 0.5
)
XG_model_gaus <- xgboost(data = X_design,
                         label = y_non_trans,
                         booster = "gbtree",
                         objective = "reg:linear",
                         max.depth = 6,
                         eta = 0.1,
                         nround = 1000,
                         subsample = 0.5,
                         colsample_bytree = 0.5
)


par(mfrow=c(1,2))
lims <- c(300,500)
plot(EN_CV, ylim = lims)
abline(h = min(EN_CV$cvm))
abline(h = min(XG_cv$evaluation_log$test_rmse_mean^2))
plot(1:XG_cv$niter, XG_cv$evaluation_log$test_rmse_mean^2, ylim = lims)
abline(h = min(EN_CV$cvm))
abline(h = min(XG_cv$evaluation_log$test_rmse_mean^2))
min(XG_cv$evaluation_log$test_rmse_mean^2) # 335.3853



pred <- exp(predict(XG_model, newdata = X_design)) - 1
pred_pois <- predict(XG_model_pois, newdata = X_design) - 1
pred_pois_pois <- exp(predict(XG_model_pois_pois, newdata = X_design)) - 1
pred_gaus <- predict(XG_model_gaus, newdata = X_design) - 1

non <- sum((pred - (exp(y) - 1))^2)
gaus <- sum((pred_gaus - (exp(y) - 1))^2)
pois <- sum((pred_pois - (exp(y) - 1))^2)
pois_pois <- sum((pred_pois_pois - (exp(y) - 1))^2)

non <- sum((log(pred) - y)^2)
gaus <- sum((log(ifelse(pred_gaus<0,1,pred_gaus)) - y)^2)
pois <- sum((log(pred_pois) - y)^2)
pois_pois <- sum((log(pred_pois_pois) - y)^2)

xpois <- rpois(1000, lambda = 5)
hist(xpois, breaks = 100, col = 8)
hist(log(xpois), breaks = 100, col = 8)

par(mfrow=c(1,2))
plot(1:XG_cv$niter, XG_cv$evaluation_log$test_rmse_mean^2)
# plot(1:XG_cv_pois$niter, XG_cv_pois$evaluation_log$test_rmse_mean^2)
plot(1:XG_cv_pois$niter, log(XG_cv_pois$evaluation_log$test_rmse_mean)^2)
