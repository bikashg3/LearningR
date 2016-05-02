# https://cran.r-project.org/web/packages/readstata13/README.html

# how to use R to open a Stata dta file with version of older than 12, including version 13 and 14

install.packages("readstata13")
library(readstata13)
datStata14 <- read.dta13("Z:/your working directory/A-Net/ANetData/ELAMath2yrMerge_A2A3.dta")
