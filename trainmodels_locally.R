
rm(list = ls())

i <- as.numeric(Sys.getenv("j"))
type <- Sys.getenv("type")
doreverse <- Sys.getenv("reverse")

#i <- 2; type <- "scatter"; doreverse <- TRUE

print(i); print(type); print(doreverse) 

epochs <- 20
batch_size <- 100 

#library(keras) 
#mnist <- dataset_mnist()
#mnist <- NULL

source("trainmodels_distr.R")
