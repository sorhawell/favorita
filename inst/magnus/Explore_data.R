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
y <- fv$train$unit_sales
X <- as.data.frame(fv$train)[!names(fv$train) %in% c("id","unit_sales")]
# X <- fv$train[, !c("id","unit_sales"), with = FALSE]
head(X)

set.seed(65846651)
use <- sample(1:nrow(X),100000)
X <- X[use,]
y <- y[use]
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


# You have omitted transactions. this might not be optimal!

## Fix X after merge
head(X)
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
                objective = "reg:linear",
                nfold = 10,
                max.depth = 2,
                eta = 0.01,
                nround = 5000,
                subsample = 0.5,
                colsample_bytree = 0.5,
                early_stopping_rounds = 100
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
# XG_model <- xgboost(data = X_design, label = y, max.depth = 4, eta = 0.01, nthread = 4, nround = 10, objective = "reg:linear")
