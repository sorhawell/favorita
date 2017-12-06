rm(list=setdiff(ls(),"fv"))
options(scipen = 99999999)

# '+' <- function(x,x2=NULL) {
#   if(is.character(x)) paste0(x,x2) else base::'+'(x,x2)
# } 
library(data.table)
library(fasttime)
library(favorita)
library(randomForest)
library(magrittr)


fv = load_fv(file = '../favorita_grocery_sales_forecasting/data/fv.rds')

set.seed(1)
samps.ind = sample(nrow(fv$train),2000)
X = fv$train[samps.ind,]  
X$onpromotion = factor(X$onpromotion)
levels(X$onpromotion) = c(levels(X$onpromotion),"na")
X$onpromotion[is.na(X$onpromotion)] = "na"
X$date = as.numeric(fastPOSIXct(X$date))
y = X$unit_sales
X = as.data.frame(X)

X = X[,(!names(X) %in% "unit_sales")]
apply(X,2,function(x) is.na(x) %>% mean)


#this model is terrible
rf = randomForest(X,y)
rf





diff_time <- difftime(Sys.time(), data_list[["train"]]$date)

hist(data_list[["train"]]$id, col = 8)
plot(data_list[["train"]]$date[sample(1:length(data_list[["train"]]$date),10000)])
hist(as.numeric(diff_time), col = 8)
hist(data_list[["train"]]$store_nbr, col = 8)

write.table(1:10, './.')
