rm(list=ls())

# install.packages("data.table")
library(data.table)

## Load all data
temp <- list.files(path = "../favorita_grocery_sales_forecasting/data/", pattern="*.csv")
temp2 <- paste0("../favorita_grocery_sales_forecasting/data/", temp)
list2env(
  lapply(setNames(temp2, make.names(gsub("*.csv$", "", temp))), 
         fread), envir = .GlobalEnv)
