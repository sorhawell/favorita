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


View(fv$train[1:1000,])
fv$train$store_nbr[1:422,]


fv25 = fv$train[fv$train$store_nbr==25,]
fv25$date2 = fastPOSIXct(fv25$date)
item.uni = unique(fv25$item_nbr)
out = by(fv25[,],fv25$date,function(df) {
  x = tapply(df$unit_sales,df$item_nbr,sum)
  reportedSales = rep(0,length(item.uni))
  names(reportedSales) = item.uni
  reportedSales[names(x)] = x
  reportedSales
}) 
out = (do.call(rbind,out))
subTotals = apply(out,2,sum)
dim(out)
out = out[,sort(subTotals,dec=T,ind=T)$ix]

View(out[,1:10])
Dates = fastPOSIXct(rownames(out))
years

library(forestFloor)
for(i in 10:1) {
  plot(yday(fastPOSIXct(rownames(out))),out[,i],main=i,col=fcol(year(Dates)))
}

fastPOSIXct(rownames(out))[sort(out[,1],dec=T,ind=T)$ix][1:100]

View(fv$holidays_events)
View(out[,1:100])

sum(out[[1]])
View(data.frame(
  names(out[[1]]),
  unname(out[[1]])
))

hist()

