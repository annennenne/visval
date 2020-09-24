###########################################################################
#### helper functions
###########################################################################

plotflow <- function(type, n, vari, mech, folder, val_split = validation_split,
                     target_size = plotsize, subset, quiet = TRUE) {
  
  generator <- image_data_generator(preprocessing_function = inception_v3_preprocess_input,
                                    validation_split = val_split)
  path <- paste(folder, type, n, vari, sep = "/")
  
  plot_flow_gen <- flow_images_from_directory(
    path,
    generator = generator,
    target_size = target_size,
    class_mode = "categorical",
    classes = c("null", mech),
    subset = subset
  )
  if (!quiet) {
	print(path)
	print("Plotflow completed!")
  }
  
  plot_flow_gen
}

makenet <- function(target_size = plotsize) {
  inlayer <- layer_input(shape = c(target_size, 3))
  
  inception <- application_inception_v3(weights = NULL, #non-pretrained weights
                                        input_tensor = inlayer,
                                        pooling = "avg",
                                        classes = 2)
  inception
}


fitnet <- function(thisnet, nepochs, batchsize, trainflow, valflow,
                   quiet = TRUE) {
  
  thisnet %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_rmsprop(decay = 1e-6), #optimizer_sgd(),
    metrics = "accuracy"
  )
  
  if (!quiet) print("Net compiled!")
  
  thisfit <- thisnet %>% fit_generator(
    # train data
    generator = trainflow,
    
    # epochs
    steps_per_epoch = as.integer((trainflow$n / batchsize) - 1), 
    epochs = nepochs, 
    
    # validation data
    validation_data = valflow,
    validation_steps = as.integer(valflow$n/batchsize),#,
    
    #no plots
    verbose = 1,# 2,
    view_metrics = 0,

workers = 1
    
  )
  if (!quiet) print("Net fitted!")
  
  thisfit
}

flowfitsave <- function(type, n, vari, mech, plotfolder, resfolder, 
                        val_split, target_size, nepochs, batch_size,
                        quiet = TRUE) {
  
 # browser()
  #make dataflows
  trainflow <- plotflow(type, n, vari, mech, plotfolder,
                        val_split, target_size,
                        subset = "training", quiet = quiet)
  valflow <- plotflow(type, n, vari, mech, plotfolder, 
                      val_split, target_size,
                      subset = "validation", quiet = quiet)
  
  #make net
  net <- makenet(target_size)
  
  #fit net
  thisfit <- fitnet(net, nepochs, batch_size, trainflow, valflow,
                    quiet = quiet)
  
  #save results
  res <- data.frame(type = type,
                    n = n,
                    vari = vari,
                    mech = mech,
                    data = rep(c("train", "val"), each = nepochs),
                    loss = c(thisfit$metrics$loss, thisfit$metrics$val_loss),
                    acc = c(thisfit$metrics$acc, thisfit$metrics$val_acc),
                    epoch = rep(1:nepochs, 2))
  
  if (!quiet) print(res)
  
  save(list = c("res"), file = paste(resfolder, "/",
                                     paste(type,  n, vari, mech, sep = "_"),
                                     ".rda", sep = ""))

  if (TRUE) {
	save(list = ls(), file = paste(resfolder, "/",
                                     paste("workspace", type,  n, vari, mech, sep = "_"),
                                     ".rda", sep = ""))
  }
  
  #return results
  res
}

#cnull <- function(...) NULL


###########################################################################
#### settings & libraries
###########################################################################

types <- rev(c("qq", "residual", "scatter"))
ns <- list(`n5` = 5, `n10` = 10, `n20` = 20, `n50` = 50)
varis <- list(`var05` = 0.5, `var1` = 1, `var2` = 2)
mechs <- c("A", "B", "C") #no null!


#thisType <- "residual"
#thisN <- "n50"
#thisVari <- "var1"
#thisMech <- "C"

plot_folder <- "./plots"
res_folder <- "./results"

#epochs <- 10
#batch_size <- 100 
#startseed <- 8101

validation_split <-  0.1 #0.1 #10% validation
plotsize <- c(141, 141) #pixels 

nCombos <- length(types)*length(ns)*length(varis)*length(mechs)

combos <- data.frame(ns = rep(names(ns), each = 3*3),
                     varis = rep(rep(names(varis), each = 3), 4),
                     mechs = rep(mechs, 3*4),
                     i = 1:(3*3*4), stringsAsFactors = FALSE)

#n.iter_sim <- as.numeric(Sys.getenv("SGE_TASK_LAST"))

if(doreverse %in% ls()) {
  if (doreverse) {
    #ns <- rev(ns)
    #varis <- rev(varis)
    #mechs <- rev(mechs)
    i <- nCombos-i
  }
}


###########################################################################
#### fit nets
###########################################################################

n <- combos[i, "ns"]
vari <- combos[i, "varis"]
mech <- combos[i, "mechs"]

combo <- paste(res_folder, "/", paste(type, n, vari, mech, sep = "_"), ".rda", sep = "")

#print(paste("seed:", startseed+100*i))
#set.seed(startseed+100*i)

starttime <- Sys.time()
    
print(paste("Reached", combo, "at time", starttime))
    
if (!file.exists(combo) || file.size(combo) < 400) {
  library("keras")#, quietly = TRUE)
  out <- flowfitsave(type, n, vari, mech, plot_folder,
                     res_folder, validation_split, plotsize,
                     epochs, batch_size, quiet = FALSE)
      #      progress <- paste(round(100*i/nCombos, 1), "% done.")
      #      print(progress) 
      print(paste("Fitted", combo))
} else {
        print(paste("Skipped", combo))
}
    #if (FALSE) {
    #   send.mail(from="annepetersen111@gmail.com",
    #      to="ahpe@sund.ku.dk",
    #      subject="Status fra Bayes",
    #      body=paste("Det går fremad med trainmodels_parallel.R. Nu er", 
    #            progress, "Mvh, Bayes."),
    #      html=T,
    #      smtp=list(host.name = "smtp.gmail.com",
    #                port = 465,
    #                user.name = "annepetersen111@gmail.com",
    #                passwd = "MegetPassword!!!",
    #                ssl = T),
    #      authenticate=T
    #      )
    #}
    #  i <<- i + 1 #only binds locally otherwise

endtime <- Sys.time()
print(paste("Time spent:", endtime - starttime))


