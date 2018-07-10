## Bayesian Model

library(rethinking)  # useful package for bayesian models


## ------------------------------------------------------ ##
##  BAYESIAN MODEL OF ITP VS NON-ITP PATIENTS
##
## -------------------------------------------------------##

# To work - the model needs to have gaussian distributed values
# to achieve this - log normalisation/transformation

## MODEL 1: ITP patients

# d1 - itp patients
# flat priors

m2 <- map(
  alist(
    plt.log ~ dnorm(mu, sigma),
    mu <- a + b*tpo.log,
    a ~ dnorm(4,2),
    b ~ dnorm(0, 10),
    sigma ~ dunif(-1,1)), 
  data = d1)

## Extraction - model 1 - ITP patients
post1 <- extract.samples(m2, n = 1000)

# Prediction for platelet counts
mu.10 <- post1$a + exp(post1$b*10)
mu.1 <- post1$a + exp(post1$b*1)
mu.150 <- post1$a + exp(post1$b*150)
mu.2 <- post1$a + exp(post1$b*2)
mu.3 <- post1$a + exp(post1$b*3)
mu.4 <- post1$a + exp(post1$b*4)
mu.5 <- post1$a + exp(post1$b*5)
mu.6 <- post1$a + exp(post1$b*6)
mu.7 <- post1$a + exp(post1$b*7)
mu.8 <- post1$a + exp(post1$b*8)
mu.9 <- post1$a + exp(post1$b*9)



## MODEL 2: non-itp patients

d2 <- list(plt.log = nitp$plt.log,
           tpo.log = nitp$tpo.log)

# flat priors

m3 <- map(
  alist(
    plt.log ~ dnorm(mu, sigma),
    mu <- a + b*tpo.log,
    a ~ dnorm(3,1),
    b ~ dnorm(0, 10),
    sigma ~ dunif(1,10)), 
  data = d2)


# extraction /prediction for model 2
post2 <- extract.samples(m3, n = 1000)


# prediction for different platelet counts
mu10 <- post2$a + exp(post2$b*10)
mu1 <- post2$a + exp(post2$b*1)
mu150 <- post2$a + exp(post2$b*150)
mu2 <- post2$a + exp(post2$b*2)
mu3 <- post2$a + exp(post2$b*3)
mu4 <- post2$a + exp(post2$b*4)
mu5 <- post2$a + exp(post2$b*5)
mu6 <- post2$a + exp(post2$b*6)
mu7 <- post2$a + exp(post2$b*7)
mu8 <- post2$a + exp(post2$b*8)
mu9 <- post2$a + exp(post2$b*9)


## reverse transformation and subset into a dataframe
df2 <- data.frame(
  "non-itp1" = exp(mu1),
  "non-itp2" = exp(mu2),
  "non-itp3" = exp(mu3),
  "non-itp4" = exp(mu4),
  "non-itp5" = exp(mu5),
  "non-itp6" = exp(mu6),
  "non-itp7" = exp(mu7),
  "non-itp8" = exp(mu8),
  "non-itp9" = exp(mu9),
  "non-itp10" = exp(mu10),
  "non-itp150" = exp(mu150),
  "itp1" = exp(mu.1) ,
  "itp2" = exp(mu.2) ,
  "itp3" = exp(mu.3) ,
  "itp4" = exp(mu.4) ,
  "itp5" = exp(mu.5) ,
  "itp6" = exp(mu.6) ,
  "itp7" = exp(mu.7) ,
  "itp8" = exp(mu.8) ,
  "itp9" = exp(mu.9) ,
  "itp10" = exp(mu.10) ,
  "itp150" = exp(mu.150),
  "number" = seq(1, 1000, 1))

# reshape dataset
df3 <- reshape(df2, varying = c(1:22), idvar = "number", direction = "long", sep = "")

## Further adjustment for plotting

df3$non.itp <- as.vector(df3$non.itp) # vector
sum(df3$non.itp == "Inf")             # remove "INF" results
colnames(df3)[2] <- "platelet_count"  # rename columns

df4 <- reshape(df3, varying = c(3:4),idvar = "tpo",direction= "long") ## further reshape to single column
head(df4)

## Form dataset
df5 <- tibble("tpo" = c(df4$non, df4$itp),
              "diagnosis" = c(rep("BMF", 11000),rep("itp", 11000)),
              "platelet_count" = rep(df4$platelet_count, 2))

## diagnosis as factor - to allow plotting
df5$d <- as.factor(df5$diagnosis)

sum(df5$tpo == "Inf")  # check infinity removed

## 25 rwos in df$tpo (due to non-itp) that have an infinite result - we must remove these
df6 <- df5[!df5$tpo == "Inf", ]  # remove "Inf" values"
df7 <- df6[!df6$tpo > 2000,]   # remove extreme values!

## further subset for only platelet counts 1, 10 and 150
df8 <- df7[df7$platelet_count == 1 | df7$`platelet count` == 10 |
             df7$platelet_count == 150,]

