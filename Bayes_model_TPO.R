## Bayesian Model

library(rethinking)  # useful package for bayesian models


## ------------------------------------------------------ ##
##  BAYESIAN MODEL OF ITP VS NON-ITP PATIENTS
##
## -------------------------------------------------------##

## model

m2 <- map2stan(
  alist(
    tpo_log ~ dnorm(mu, sigma),
    mu <- a + (b*plt_log),
    a ~ dnorm(0.01,10),
    b ~ dnorm(0.01,10),
    tpo <- exp(tpo_log),
    plt <- exp(plt_log),
    sigma ~ dunif(-1,10)), 
  data = d3, warmup = 500, iter = 1e5)


## predict outcomes
plt <- seq(from=1,to=200,length.out=200) ## plt counts you want to predict
new_data <- data.frame(plt_log=log(plt))  ## new dataset of plt counts
lambda <- link(m2,data=new_data,n=9000)   ## project model parameters - project 9000

lambda.mu.mean <- apply(lambda$mu,2,mean)  # take average of each column to get mean
lambda.mu.HPDI <- apply(lambda$mu,2,HPDI)  # highest posterior density (HPDI) and percentile (PI)
head(lambda.mu.mean)
tt.sim <- sim(m2,data=new_data,n=9000)  # simulate out the model to get simulated curves
tt.HPDI <- apply(tt.sim,2,HPDI)  ## highest posterior density simulations
tt.HPDI

## plot predictions - 
par(mfrow = c(1,1))
par(mar = c(4,4,4,4))
plot( tpo_log ~ plt , data=d3 , pch=16 , col=rangi2,
      ylab = "log TPO pg/ml", 
      xlab = (expression(paste("Platelet count (",x10^{9},"/L)"))), mgp = c(2,0.5,0))
lines( plt , lambda.mu.mean )
shade( lambda.mu.HPDI , plt )
shade( tt.HPDI , plt )


## pull results into a dataframe
head(tt.HPDI)

df4 <- tt.sim[,c(1,2,3,4,5,6,7,8,9,10, 20, 30, 40, 50, 60, 70, 80, 90, 150, 200)]
df4 <- as.data.frame(df4)
head(df4)
n1 <-sprintf("Platelet_count%s", c(1,2,3,4,5,6,7,8,9,10, 20, 30, 40, 50, 60, 70, 80, 90, 150, 200))
colnames(df4) <- n1  # labels


#write.table(df4, "df4.tsv", col.names = NA, quote = F, sep = "\t")

# reshape
df5 <- reshape(df4, varying = c(1:20), idvar = "number", direction = "long", sep = "")

colnames(df5) <- c("Platelet_count", "tpo", "number")  # colnames
#write.table(df5, "df5.tsv", col.names = NA, quote = F, sep = "\t")

df5$tpo_e <- exp(df5$tpo)# add an tpo (instead of log tpo)

## density plot
ggdensity(df5, x = "tpo", y = "..density..", combine = T, rug = TRUE, fill = "Platelet_count",
          color = 'Platelet_count', palette = "simpsons", add = 'mean')

df5$Platelet_count <- as.character(df5$Platelet_count)

## subset a few levels
df6 <- df5[df5$Platelet_count == 1 | df5$Platelet_count == 10 | df5$Platelet_count == 20 | df5$Platelet_count == 30 | 
             df5$Platelet_count == 40 | df5$Platelet_count == 50 | df5$Platelet_count == 60 | 
             df5$Platelet_count == 70 | df5$Platelet_count == 80 | df5$Platelet_count == 90 |
             df5$Platelet_count == 100,]


ggdensity(df6, x = "tpo_e", y = "..density..", combine = T, rug = TRUE, fill = "Platelet_count",
          color = 'Platelet_count', palette = "grey", add = 'mean', xlim = c(0, 1000))

dipt1 <- ggdensity(df6, x = "tpo_e", y = "..density..", combine = T, rug = TRUE, fill = "Platelet_count",
                   color = 'Platelet_count', palette = "grey", add = "median", xlab = "TPO (pg/ml)",
                   label.select = list(top.up = 200, 20, 10, 7, 5, 3, top.down = 1), xlim = c(0, 1000))


## plot

par(mar = c(4,4,4,4))
plot( tpo_log ~ plt , data=d3 , pch=16 , col=rangi2,
      ylab = "log TPO pg/ml", 
      xlab = (expression(paste("Platelet count (",x10^{9},"/L)"))), mgp = c(2,0.5,0))
lines( plt , lambda.mu.mean )
shade( lambda.mu.HPDI , plt )

## make a new dataframe of plt versys log tpo
d9 <- data.frame(
  "plt" = plt,
  "log_tpo" = lambda.mu.mean
)

head(d9)
class(d9)

shade( tt.HPDI , plt )
head(tt.HPDI)

head(lambda.mu.HPDI)
n1 <- sprintf("Platelet_count%s", c(1:200))

d7 <- as.data.frame(tt.HPDI)
dim(d7)
colnames(d7) <- n1
rownames(d7) <- c("lower", "upper")

d8 <- reshape(d7, varying = c(1:200), idvar = "number", direction = "long", sep = "")
colnames(d8) <- c("Platelet_count", "tpo_log", "dim")
head(d8)
d8$tpo <- exp(d8$tpo_log)
head(d8)

## write the dataframes
d5 <- reshape(d4, idvar = 1:200,direction =  "long")
write.table(d5, "dim_d5.tsv", col.names = NA, quote = F, sep = "\t")
write.table(d8, "dim_d8.tsv", col.names = NA, quote = F, sep = "\t")





