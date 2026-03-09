# cleaning
rm(list = ls())

library(dplyr)

# read tsv
data1 <- read.table("./ST_fitted_dists_families_65.tsv", sep = "\t", header = TRUE)
data2 <- read.table("./ST_fitted_dists_orders_35.tsv", sep = "\t", header = TRUE)

data1 <- select(data1,MCMCtree.calib.rounded) 
data2 <- select(data2,MCMCtree.calib.rounded)

# Filter rows based on row names using dplyr
data1 <- data1 %>%
  slice(which(row.names(.) %in% c("trochodendraceae", "altingiaceae")))

data2 <- data2 %>%
  slice(which(row.names(.) %in% c("asterales")))
