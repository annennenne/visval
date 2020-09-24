setwd("~/bshome/visval")

allres <- data.frame(type = NA, n = NA, 
                     vari = NA, mech = NA,
                     data = NA, loss = NA, 
                     acc = NA, epoch = NA)

files <- list.files(path="results", 
                    pattern="*.rda", 
                    full.names=TRUE, 
                    recursive=FALSE)

for (f in files) {
  load(f)
  allres <- rbind(allres, res)
}

allres <- allres[-1, ]

allres$type <- as.character(allres$type)
allres$type[allres$type == "qq"] <- "QQ"
allres$type[allres$type == "residual"] <- "Residual"
allres$type[allres$type == "scatter"] <- "Scatter"
allres$type <- factor(allres$type, levels = c("Scatter", "QQ", "Residual"))


library(data.table)
allres_dt <- data.table(allres[allres$data == "val",])


allres_dt <- allres_dt[, sort(acc, decreasing = TRUE)[2], by = .(type, n, vari, mech)]

allres_mini <- data.frame(allres_dt, stringsAsFactors =FALSE)
head(allres_mini)
allres_mini$n <- as.numeric(substring(allres_mini$n, 2))


library(ggplot2)

cbfriendly <- c("#E69F00", "#56B4E9", "#009E73")

ggplot(allres_mini, aes(x = n, y = V1, col = vari)) +
  geom_hline(yintercept = 0.5, color = "darkgrey", lty = "dotted") + 
  geom_point() + 
  geom_line(lty = "dashed") + 
  facet_grid(type ~ mech) +
  scale_x_continuous(trans = "log2", breaks = c(5, 10, 20, 50)) + 
  scale_color_manual("Variance", values = cbfriendly, 
                     labels = list(`var05` = 0.5, `var1` = 1, 
                                                   `var2` = 2)) +
  scale_y_continuous(breaks = seq(0.5, 1, 0.1), limits = c(0.5, 1.0)) +
  ylab("2nd best validation accuracy") +
  theme_bw() 

  

allres[allres$n == "n5" & allres$vari == "var05" & allres$mech == "A", ]



ggplot(allres[allres$data == "val",], aes(x = epoch, y = acc,
                                          col = vari,
                                          lty  = n)) +
  geom_line() + 
  facet_grid(mech ~ type) +
  ylab("Validation accuracy") +
  xlab("Epoch") +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  theme_bw() + 
  scale_color_manual("Variance", values = cbfriendly, 
                     labels = list(`var05` = 0.5, `var1` = 1, 
                                   `var2` = 2)) +
  scale_linetype_discrete(labels = list(`n10` = 10, `n20` = 20, 
                                   `n5` = 5, `n50` = 50),
                          limits = c("n5", "n10", "n20", "n50"))


ggplot(allres[allres$data == "val",], aes(x = epoch, y = loss,
                                          col = vari,
                                          lty  = n)) +
  geom_line() + 
  facet_grid(mech ~ type) +
  ylab("Validation loss") +
  xlab("Epoch") +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  theme_bw() + 
  scale_color_manual("Variance", values = cbfriendly, 
                     labels = list(`var05` = 0.5, `var1` = 1, 
                                   `var2` = 2)) +
  scale_linetype_discrete(labels = list(`n10` = 10, `n20` = 20, 
                                        `n5` = 5, `n50` = 50),
                          limits = c("n5", "n10", "n20", "n50"))


ggplot(allres[allres$data == "train",], aes(x = epoch, y = loss,
                                          col = vari,
                                          lty  = n)) +
  geom_line() + 
  facet_grid(mech ~ type) +
  ylab("Training loss") +
  xlab("Epoch") +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  theme_bw() + 
  scale_color_manual("Variance", values = cbfriendly, 
                     labels = list(`var05` = 0.5, `var1` = 1, 
                                   `var2` = 2)) +
  scale_linetype_discrete(labels = list(`n10` = 10, `n20` = 20, 
                                        `n5` = 5, `n50` = 50),
                          limits = c("n5", "n10", "n20", "n50"))


###########

#load("./results/workspace_scatter_n5_var05_A.rda")




ggplot(allres[allres$data == "val",],
       aes(x = epoch, y = acc, col = n, lty = vari)) +
  geom_line() + 
  facet_grid(type ~ mech) +
  ylab("Validation loss") +
  xlab("Epoch") +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  theme_bw() + 
  scale_color_manual("n", values = cbfriendly, 
                     labels = list(`n5` = 5, `n10` = 10, 
                                   `n20` = 20, `n50` = 50),
                     limits = c("n5", "n10", "n20", "n50")) + 
  theme(legend.position = "bottom")

