# rm(list=ls())
options(scipen = 99999999)

library(fasttime)
library(favorita)
library(glmnet)
fv <- fv_full <- load_fv(file = '../favorita_grocery_sales_forecasting/data/fv.rds')
fv$train$date <- fastPOSIXct(fv$train$date)


str(fv$train)
summary(fv$train)
head(fv$train)

y <- fv$train$unit_sales
X <- fv$train[!names(fv$train) %in% c("id","unit_sales")]
head(X)

use <- sample(1:nrow(X),100000)
X <- X[use,]
y <- y[use]


summary(X)

X2 <- data.frame(X)
X2$date <- as.numeric(X$date)
X2$store_nbr <- factor(X2$store_nbr)
X2$onpromotion <- factor(X2$onpromotion) # remember NA
levels(X2$onpromotion) = c(levels(X2$onpromotion),"na")
X2$onpromotion[is.na(X2$onpromotion)] = "na"
# X2 <- sparse.model.matrix(~.-1, data = X2 )
# dim(X2)
X2 <- sparse.model.matrix(~.-1, data = X2, contrasts.arg = lapply( X2[ sapply(X2, is.factor)], contrasts, contrasts=FALSE))
dim(X2)
head(X2)
str(X2)

head(X2)

alpha <- 0.95
set.seed(6848684)
EN_CV <- cv.glmnet(X2, y, intercept = TRUE, alpha = alpha)
plot(EN_CV)
# consider time trend

EN <- glmnet(X2, y, intercept = TRUE, alpha = alpha)
mean((y-mean(y))^2)
coef(EN, s = -5)

plot(X$date[sample(1:length(X$date),10000)])
hist(as.numeric(X$date), col = 8)
hist(fv$train$store_nbr, col = 8)

write.table(1:10, './.')
