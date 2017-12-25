rm(list=ls()[ls()!="fv"])
##a full data array would be 
nrow(fv$items)
length(fastPOSIXct(unique(fv$train$date)))

nrow(fv$stores)
#y2015.uni.ind = Times >= fastPOSIXct("2015-01-01") & Times < fastPOSIXct("2016-01-01") 

make.sales.m = function(train, year="2015") {
  y20xx.ind = substr(train$date,1,4) == year
  train20xx = train[y20xx.ind,]
  Times = fastPOSIXct((train20xx$date))
  dayMatch = match(train20xx$date,unique(train20xx$date))
  train20xx.uim = t(train20xx[,c("unit_sales","item_nbr")])
  
  allDays  = 1:365
  tr.byDay = lapply(allDays, function(iday) {
    print(iday)
    ind = which(dayMatch==iday)
    thisDay = train20xx.uim[,ind]
    attr(thisDay,"date") = train20xx$date[ind[1]]
    thisDay
  })
  
  allItems = sort(unique(train$item_nbr))
  salePerItem = lapply(tr.byDay, function(df) {
    out = df[1,match(allItems,df[2,])]
    if(length(out)==0) return(rep(NA,length(allItems)))
    out
  })
  sales.m = do.call(cbind,salePerItem)  
  dimnames(sales.m)  = list( allItems,sapply(tr.byDay,attr,"date"))
  sales.m
}

sales2014.m = make.sales.m(fv$train,year="2014")
sales2015.m = make.sales.m(fv$train,year="2015")
sales2016.m = make.sales.m(fv$train,year="2016")

pos2015 = fastPOSIXct(sort(unique(train$date[y20xx.ind])))
fDay = weekdays( fastPOSIXct(dimnames(sales2014.m)[[2]]))[-1:2]
weekdays( fastPOSIXct(dimnames(sales2014.m)[[2]]))[-1:-2][1:7]
weekdays( fastPOSIXct(dimnames(sales2015.m)[[2]]))[-c(1,365)][1:7]
weekdays( fastPOSIXct(dimnames(sales2016.m)[[2]]))[-(364:365)][1:7]

M14  = t(apply(sales2014.m,1,filter,filter=rep(0.1,10)))[,-1:-2]
M15  = t(apply(sales2015.m,1,filter,filter=rep(0.1,10)))[,-c(1,365)]
M16  = t(apply(sales2016.m,1,filter,filter=rep(0.1,10)))[,-c(364,365)]

Col = factor(wday(fastPOSIXct(dimnames(sales2014.m)[[2]])[-1]))
plot(apply(M14,2,mean,na.rm=T),apply(M15,2,mean,na.rm=T),col=fcol(1:363))
plot(apply(M14,2,mean,na.rm=T),apply(M16,2,mean,na.rm=T),col=fcol(1:364))
plot(apply(M15,2,mean,na.rm=T),apply(M16,2,mean,na.rm=T),col=fcol(1:364))


plot(  apply(M14,2,mean,na.rm=T),type="l")
points(apply(M15,2,mean,na.rm=T),type="l",col="red")
points(apply(M16,2,mean,na.rm=T),type="l",col="green")


prepareClust = function(m) {
  m[is.na(m)] = 0
  any.obs = apply(m,1,function(x) any(x>1))
  m = t(apply(m[any.obs,],1,function(x) x/sum(x)))
  list(m=scale(m),aobs=which(any.obs))
}

m14 = prepareClust(M14)
h14 = hclust.vector(m14$m,method = "ward")
plot(h14)
rect.hclust(h14, k=8, border = 1:8+1)
c14 = cutree(h14,8)


m15 = prepareClust(M15)
h15 = hclust.vector(m15$m,method = "ward")
c15 = cutree(h15,6)

obs.item = sort(unique(c(names(c15),names(c14))))
df = data.frame(c14=c14[match(obs.item,names(c14))],
                c15=c15[match(obs.item,names(c15))])
table(apply(df,1,paste,collapse="-"))
plot(h15)

m16 = prepareClust(M16)
h16 = hclust.vector(m16$m,method = "ward")
plot(h16)


M14[is.na(M14)] = 0
any.obs = apply(M14,1,function(x) any(x>1))
M14.n = t(apply(M14[any.obs,],1,function(x) x/sum(x)))
CM = cor(t(M14[any.obs,]))
hcl = hclust.vector(,method = "ward")
plot(hcl, labels = FALSE)
rect.hclust(hcl, k=12, border = 1:12+1)
View(M14.n)
