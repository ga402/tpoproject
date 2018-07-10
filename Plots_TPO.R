## TPO Bayesian plotting

# subset for only platelet counts 1, 10 and 150
df8 <- df7[df7$platelet_count == 1 | df7$`platelet count` == 10 |
             df7$platelet_count == 150,]

#################################  DENSITY PLOTS ###########################################

ggdensity(df8, x = "tpo", y = "..density..", combine = T, rug = TRUE, fill = "diagnosis",
          color = 'platelet_count', palette = "simpsons")


ggdensity(df8, x = "tpo", y = "..density..", combine = T, rug = TRUE, fill = "diagnosis",
          color = 'platelet_count', palette = "grey")


ggdensity(df8, x = "tpo", y = "..density..", combine = T, rug = TRUE, fill = "diagnosis",
          color = 'platelet_count', palette = "jco")

ggdensity(df8, x = "tpo", y = "..density..", combine = T, rug = TRUE, fill = "diagnosis",
          color = 'platelet_count', palette = "aaas")

ggdensity(df8, x = "tpo", y = "..density..", combine = T, rug = TRUE, fill = "diagnosis",
          color = 'platelet_count', palette = "rickandmorty")


ditp1 <- ggdensity(df8, x = "tpo", y = "..density..", combine = T, rug = TRUE, fill = "diagnosis",
                   color = 'platelet_count', palette = "grey", xlab = "TPO (pg/ml)")

ditp2 <- ggdensity(df8, x = "tpo", y = "..density..", combine = T, rug = TRUE, fill = "diagnosis",
                   color = 'platelet_count', palette = "RdBu")

ditp3 <- ggdensity(df8, x = "tpo", y = "..density..", combine = T, rug = TRUE, fill = "diagnosis",
                   color = 'platelet_count', palette = "lancet")

ditp4 <- ggdensity(df8, x = "tpo", y = "..density..", combine = T, rug = TRUE, fill = "diagnosis",
                   color = 'platelet_count', palette = "npg")


#################################  SMOOTHED PLOTS ###########################################

df8 <- read.table(file="~/Dropbox/005_ITP_projects/003 TPO/R commands for TPO/df8.tsv",
                  sep = '\t', header = TRUE)

library(ggthemes)
library(ggpubr)
library(ggsci)
library(cowplot)

#### produce dataframes for plotting

#smooth1 and smooth2 (sm1 and sm2)  - for the log_TPO and platelet counts
sm1_itp <- data.frame("log_TPO" = itp$log10tpo,
                      "platelet_count" = itp$plt)

sm2_nonitp <- data.frame("logTPO" = nitp$log10tpo,
                         "plateletcount" = nitp$plt)

## to try and keep the same platelet count range - smooth_adjusted
sm1_a <- sm1_itp[sm1_itp$platelet_count < 350,]
sm2_a <- sm2_nonitp[sm2_nonitp$plateletcount < 350,]

sm2_a2 <- sm2_a[1:11,] ## note- Number 14 - appears to be an outlier - to remove

############# -------------------------------------
#### To have dataframe without log - THIS IS NOT used

smooi <- data.frame("TPO" = tpon$tpo,
                    "Platelet_count" = tpon$plt)
smoof <- data.frame("Tpo" = nitp$tpo,
                    "Plateletcount" = nitp$plt)
smooi <- smooi[!smooi$Platelet_count > 150,]  # required smaller range
smoof <- smoof[!smoof$Plateletcount > 150,]   #

############### --------------------------------------

## PLOT SMOOTHED PLOTS - for both ITP and non-ITP

## FOR WITHOUT LOGs
t <- ggplot(smooi, aes(Platelet_count, TPO)) +
  geom_point(size = 3, colour = "blue", alpha = 0.5)  +
  geom_smooth(se=F, size=1.5, span =1, colour = "darkblue")  +
  theme_pubr() + scale_color_nejm() #+ scale_y_log10()

t1 <- t +   geom_smooth(data = smoof, mapping = aes(Plateletcount, Tpo),
                        method = "loess", size = 1.5, se = F, span = 1,
                        colour= "red4")

t1 <- t1 + geom_point(data = smoof, mapping = aes(Plateletcount, Tpo),
                      size = 3, alpha = 0.5, colour = "firebrick2")
t1 <- t1 +  theme(legend.position="none") + ylab(expression(paste("TPO (pg/ml)"))) +
  xlab(expression(paste("Platelet count ",x10^{9},"/L")))
t1

########### REPEAT PLOTS WITH LOGs ##########################################

## now to plot
g <- ggplot(sm1_a, aes(platelet_count, log_TPO)) +
  geom_point(colour = "blue", size = 3) +
  geom_smooth(colour = "blue",se = T, method = "loess", span = 1)  + theme_pubr()
# selected span of 1
g

g1 <- g +   geom_smooth(data = sm2_a2, mapping = aes(plateletcount, logTPO),
                        method = "loess",
                        colour = "red", span = 1)
g1
g1 <- g1 + geom_point(data = sm2_a2, mapping = aes(plateletcount, logTPO), colour = "red",
                      size = 3)
g1 <- g1 +  theme(legend.position="none") + ylab(expression(paste("",Log^{10},"TPO (pg/ml)"))) +
  xlab(expression(paste("Platelet count ",x10^{9},"/L")))
g1


############ TO COMBINE PLOTS INTO FINAL OUTPUT #########################

### COWPLOT ##

## Now to to combine the two plot!


library(cowplot)
ggdraw() +
  draw_plot(ditp1 + theme(legend.justification = "bottom"), 0, 0, 1, 1) +
  draw_plot(g1 +
              theme(legend.justification = "top"), 0.48, 0.4, 0.5, 0.48) +
  draw_plot_label(c("A.", "B."), c(0, 0.45), c(1, 0.9), size = 22)

ggdraw() +
  draw_plot(ditp3 + theme(legend.justification = "bottom"), 0, 0, 1, 1) +
  draw_plot(t1 +
              theme(legend.justification = "top"), 0.48, 0.4, 0.5, 0.48) +
  draw_plot_label(c("A", "B"), c(0, 0.4), c(1, 0.8), size = 15)


ggdraw() +
  draw_plot(ditp4 + theme(legend.justification = "bottom"), 0, 0, 1, 1) +
  draw_plot(t1 +
              theme(legend.justification = "top"), 0.48, 0.4, 0.5, 0.48) +
  draw_plot_label(c("A", "B"), c(0, 0.4), c(1, 0.8), size = 15)
