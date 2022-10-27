
library(devtools)
library(doParallel)
library(arrow)
library(sparklyr)

# options
options(scipen = 999)

# set seed
set.seed(123)

# parallel computing
cl <- makeCluster(4)
registerDoParallel(cl)
getDoParWorkers()


getwd()
# load all functions from funs folder - invisible hides output
invisible(sapply(paste0('./R/', list.files('./R')), source, .GlobalEnv))


