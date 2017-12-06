rm(list=ls())
options(scipen = 99999999)

library(fasttime)
library(favorita)
load_fv(file = '../favorita_grocery_sales_forecasting/data/fv.rds')

data_list[["train"]]$date <- fastPOSIXct(data_list[["train"]]$date)

str(data_list[["train"]])
summary(data_list[["train"]])

diff_time <- difftime(Sys.time(), data_list[["train"]]$date)

hist(data_list[["train"]]$id, col = 8)
plot(data_list[["train"]]$date[sample(1:length(data_list[["train"]]$date),10000)])
hist(as.numeric(diff_time), col = 8)
hist(data_list[["train"]]$store_nbr, col = 8)

write.table(1:10, './.')
