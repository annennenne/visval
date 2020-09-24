
###########################################################################
#### settings & libraries
###########################################################################

types <- c("qq", "scatter", "residual")
ns <- list(`n5` = 5, `n10` = 10, `n20` = 20, `n50` = 50)
varis <- list(`var05` = 0.5, `var1` = 1, `var2` = 2)
mechs <- c("null", "A", "B", "C")
nplots <- 5000
i0 <- 0
startseed <- 123456

library(foreach)
library(doMC)

serverdir <- "/home/ifsv/zms499/visval"
setwd(serverdir)

folder <- "./plots"

source("makeplots_functions.R")


###########################################################################
#### simulate data and store plots
###########################################################################

#always start with the same seed across type-n-var-mech combos,
#but ensure same seed is not reused within a combo by adding
#2*i0

registerDoMC(1) #detectCores()[1]-1)

starttime <- Sys.time()

foreach(type = types, .combine = cnull) %:%  
  foreach(n = names(ns), .combine = cnull) %:%
    foreach(vari = names(varis), .combine = cnull) %:%
      foreach(mech = mechs, .combine = cnull) %dopar% {
        library(ggplot2)
        #set.seed(startseed + 2*i0 + 1234) 
        simsaveplot(nplots = nplots, type = type,
                    n = n, vari = vari, mech = mech, 
                    folder = folder,
                    i0 = i0, mode = "lookfirst", quiet = TRUE) #, mode = "delete")
        print(paste("mech ", mech, ", ", vari, ", ", n, 
                    ", ", type, " is DONE.", sep = ""))
      }
    
endtime <- Sys.time()

endtime - starttime


###########################################################################
#### old code not in use below
###########################################################################

#registerDoMC(detectCores()[1]-1)#
#
#foreach(i = 1:3) %:%
#  foreach(j = letters[1:3]) %dopar% {
#    print(paste(i, j))
#  }
