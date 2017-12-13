# rm(list=ls())
options(scipen = 99999999)

library(fasttime)
library(favorita)
library(glmnet)
fv <- fv_full <- load_fv(file = '../favorita_grocery_sales_forecasting/data/fv.rds')
fv$train$date <- fastPOSIXct(fv$train$date)

fv$train$holidays_events <- fv_full$holidays_events$type[match(fv_full$train$date, fv_full$holidays_events$date )]

str(fv$train)
# summary(fv$train)
head(fv$train)

y <- fv$train$unit_sales
X <- fv$train[!names(fv$train) %in% c("id","unit_sales")]
head(X)






set.seed(65846651)
use <- sample(1:nrow(X),10000)
X <- X[use,]
y <- y[use]
head(X)

table(X$holidays_events, useNA = "always")

hist(y[X$item_nbr==sample(X$item_nbr,1)], col = 8, breaks = seq(min(y),max(y),1), xlim = c(0,50))
hist(log(y+0.01), col = 8, breaks = seq(min(y),max(y),0.1), xlim = c(0,10))


summary(X)

X2 <- data.frame(X)
X2$date <- as.numeric(X$date)
X2$store_nbr <- factor(X2$store_nbr)
X2$item_nbr <- factor(X2$item_nbr)
X2$onpromotion <- factor(X2$onpromotion) # remember NA
levels(X2$onpromotion) = c(levels(X2$onpromotion),"na")
X2$onpromotion[is.na(X2$onpromotion)] = "na"
X2$holidays_events <- factor(X2$holidays_events) # remember NA
levels(X2$holidays_events) = c(levels(X2$holidays_events),"na")
X2$holidays_events[is.na(X2$holidays_events)] = "na"

# X2 <- sparse.model.matrix(~.-1, data = X2 )
# dim(X2)
X_design <- sparse.model.matrix(~.-1 + item_nbr*holidays_events + onpromotion, data = X2, contrasts.arg = lapply( X2[ sapply(X2, is.factor)], contrasts, contrasts=FALSE))
dim(X_design)
head(X_design)

colnames(X_design)

alpha <- 0.001
set.seed(6848684)
EN_CV <- cv.glmnet(X_design, y, intercept = TRUE, alpha = alpha, nfolds = 10, lambda = exp(seq(-2,10,0.05)) )
# y[y<0] <- 0
# EN_CV <- cv.glmnet(X_design, y, intercept = TRUE, alpha = alpha, family = "poisson")
plot(EN_CV)
# consider time trend

# Store number (or stores) matter
EN <- glmnet(X_design, y, intercept = TRUE, alpha = alpha)
# EN <- glmnet(X_design, y, intercept = TRUE, alpha = alpha, family = "poisson")
mean((y-mean(y))^2)
coef(EN, s = EN_CV$lambda.min)

plot(X$date[sample(1:length(X$date),10000)])
hist(as.numeric(X$date), col = 8)
hist(fv$train$store_nbr, col = 8)

write.table(1:10, './.')
