## DATA cleaning

# read dataset
tpo <- read.csv("~/Dropbox/005_ITP_projects/003 TPO/TPO dataset/tpo.csv")

#
nitp <- tpo[!tpo$diag == "itp",]  # those non-itp patients
itp <- tpo[tpo$diag == "itp",]    # itp patients
itp <- itp[itp$tpo >0,]           # TPO levels > 0 in ITP patients

## cleaningn data - removing the values that equal zero -> replacing with 1 (only 1 samples)
## (zero's not managed by model)

itp$plt == 0 ## shows number 62
itp$plt[62] <- 1

# Basic graphs - smoothed spline

plot(tpon$plt, log(tpon$tpo,10), xlab = "Platelet count", ylab = expression(log^10 ~ TPO))
points(itpn$plt, log(itpn$tpo,10), col = "blue", pch = 3)
points(nitp$plt, log(nitp$tpo, 10), col = "red", pch = 2)
f1 <- smooth.spline(nitp$plt, log(nitp$tpo, 10), spar = 1)
f2 <- smooth.spline(itpn$plt, log(itpn$tpo,10), spar = 1)
lines(f1, col = "red", lwd = 3)
lines(f2, col =  "blue", lwd = 3)
title(main = "TPO (log10) against platelet count")
text(x = 400, y = 2, labels = "Non ITP patients", col = "red")     
text(x = 400, y = 1.25, labels = "ITP patients", col = "blue")




# To produce dataframes for graphs
sm1_itp <- data.frame("log_TPO" = itp$log10tpo, 
                      "platelet_count" = itp$plt)



sm2_nonitp <- data.frame("logTPO" = nitp$log10tpo, 
                         "plateletcount" = nitp$plt)
