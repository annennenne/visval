
###########################################################################
#### helper functions
###########################################################################

simdata <- function(n, sd, mech) { #, seed = NULL) {
  #  if (!is.null(seed)) set.seed(seed) 
  x <- rnorm(n, sd = sd)
  eps <- rnorm(n)
  
  if (mech == "null") y <- x + eps
  if (mech == "A") y <- exp(x) + eps
  if (mech == "B") y <- x + exp(eps)
  if (mech == "C") y <- exp(x + eps)
  
  x <- (x - mean(x))/sd(x)
  y <- (y - mean(y))/sd(y)
  
  data.frame(x = x, y = y)
}

makesaveplot <- function(data, type, path, i = NULL, filename, 
                         size = 5, dpi = 72, returnplot = FALSE, 
                         dosave = TRUE, ...) {
  m <- lm(y ~ x, data)
  
  if (type == "qq") {
    p <- ggplot(data.frame(r = rstandard(m)), aes(sample = r)) + 
      geom_qq() + 
      geom_abline()
  } else if (type == "residual") {
    p <- ggplot(data.frame(x = data$x, r = rstandard(m)),
                aes(x = x, y = r)) +
      geom_point() + 
      geom_hline(yintercept = 0)
  } else { #case: type == "scatter"
    pars <- coef(m) 
    p <- ggplot(data, aes(x = x, y = y)) +
      geom_point() + 
      geom_abline(intercept = pars[1], slope = pars[2])
  }
  
  #remove labels, bw theme
  p <- p + xlab(NULL) + ylab(NULL) + theme_bw()
  
  #save
  if (dosave) {
    R.devices::suppressGraphics({
	  ggsave(filename = filename,
	           plot = p, dpi = dpi, width = size, height = size, units = "cm",
	           path = path, type = "cairo")
	  })
  }
  
  if (returnplot) return(p)
}

#simsaveplot <- function(nplots, type, n, vari, mech, folder, i0,
#                        quiet = TRUE, mode = "plot", device = "png",
#                        ...) {
#  path <- paste(folder, type, n, vari, mech, "/", sep = "/")
#  
#  for (i in 1:nplots) {
#    filename <- paste("p", i0 + i, ".", device, sep = "")
#    if (mode %in% c("delete", "replace")) { 
#      unlink(paste(path, "/", filename, sep = ""))
#    }
#    if (mode == "plot") {
#      if (!file.exists(paste(path, "/", filename, sep = ""))) {
#        data <- simdata(n = ns[[n]], sd = sqrt(varis[[vari]]), mech) 
#        makesaveplot(data, type, path, i0 + i, filename, ...)
#      }
 #   } 
 #   if (!quiet) print(paste(i, "done."))
 # }
#}

simsaveplot <- function(nplots, type, n, vari, mech, folder, i0,
                        quiet = TRUE, mode = "plot", device = "png",
                        ...) {
  path <- paste(folder, type, n, vari, mech, sep = "/")
  
  for (i in 1:nplots) {
    makenew <- FALSE
    filename <- paste("p", i0 + i, ".", device, sep = "")
    fileaddr <- paste(path, "/", filename, sep = "")
    if (mode %in% c("lookfirst", "checkfiles")) {
      if (!file.exists(fileaddr)) {
        makenew <- TRUE
      } else if (file.size(fileaddr) < 1000) {
        makenew <- TRUE
      }
     if (mode == "lookfirst") {
	if (makenew) print(paste("making new:", fileaddr))
     } else {
	if (makenew) print(paste("problem:", fileaddr))
        makenew <- FALSE
     }
      #a <- tryCatch(capture.output(imager::load.image(fileaddr)), 
      #              error = function(e) TRUE)
    }
    if (mode %in% c("delete", "replace") | makenew) { 
      unlink(fileaddr)
    }
    if (mode %in% c("plot", "replace") | makenew) {
      if (!file.exists(fileaddr)) {
        data <- simdata(n = ns[[n]], sd = sqrt(varis[[vari]]), mech) 
        makesaveplot(data, type, path, i0 + i, filename, ...)
      }
    } 
    if (!quiet) print(paste(i, "done."))
  }
}


cnull <- function(...) NULL
