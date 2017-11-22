# rm(list=ls())

# install.packages("fasttime")
library(data.table)
library(fasttime)

## Load all data in a list
temp <- list.files(path = "../favorita_grocery_sales_forecasting/data/", pattern="*.csv")
temp2 <- paste0("../favorita_grocery_sales_forecasting/data/", temp)
data_list <- lapply(temp2, fread)
names(data_list) <- gsub("*.csv$", "", temp)
data_list[["train"]]$date <- fastPOSIXct(data_list[["train"]]$date)


## push directly to global environmet:
# list2env(
#   lapply(setNames(temp2, make.names(gsub("*.csv$", "", temp))), 
#          fread), envir = .GlobalEnv)