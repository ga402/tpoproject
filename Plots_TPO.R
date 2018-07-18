## TPO Bayesian plotting

## ggplot 
g1 <- ggplot(d3, aes(plt, tpo_log)) + xlim(0,200) +
  geom_point(colour = "blue", size = 5, alpha = 0.8)

g1 <- g1 + geom_line(data=d5, 
                     aes(x=Platelet_count, y=tpo_log, colour=dim), colour = "grey10", alpha = 0.6)
g1 <- g1 +  geom_line(data=d8,
                      aes(x=Platelet_count, y=tpo_log, colour=dim), colour = "grey70", alpha = 0.4)

g1 <- g1 +  geom_line(data=d9,
                      aes(x=plt, y=log_tpo), colour = "grey1", alpha = 0.9)


g1 <- g1 +  theme(legend.position="none") + ylab(expression(paste("TPO"[log]," (pg/ml)"))) + 
  xlab(expression(paste("Platelet count (",x10^{9},"/L)")))


g1




### density plots
## library(ggpubr)

dipt1 <- ggdensity(df6, x = "tpo", y = "..density..", combine = T, rug = TRUE, fill = "Platelet_count",
                   color = 'Platelet_count', palette = "grey", add = "median", xlab = expression(paste("TPO"[log]," (pg/ml)")),
                   label.select = list(top.up = 200, 20, 10, 7, 5, 3, top.down = 1), xlim = c(0,16))



dipt2 <- ggdensity(df6, x = "tpo", y = "..density..", combine = T, rug = TRUE, fill = "Platelet_count",
                   color = 'Platelet_count', palette = "lancet", add = "median", xlab = expression(paste("TPO"[log]," (pg/ml)")),
                   label.select = list(top.up = 200, 20, 10, 7, 5, 3, top.down = 1), xlim = c(0, 16))


dipt3 <- ggdensity(df6, x = "tpo", y = "..density..", combine = T, rug = TRUE, fill = "Platelet_count",
                   color = 'Platelet_count', palette = "jco", add = "median",  xlab = expression(paste("TPO"[log]," (pg/ml)")),
                   label.select = list(top.up = 200, 20, 10, 7, 5, 3, top.down = 1), xlim = c(0, 16))


dipt4 <- ggdensity(df6, x = "tpo", y = "..density..", combine = T, rug = TRUE, fill = "Platelet_count",
                   color = 'Platelet_count', palette = "npg", add = "median",  xlab = expression(paste("TPO"[log]," (pg/ml)")),
                   label.select = list(top.up = 200, 20, 10, 7, 5, 3, top.down = 1), xlim = c(0, 16))


########### TPO at different plt counts

dipt1 <- ggdensity(df6, x = "tpo", y = "..density..", combine = T, rug = TRUE, fill = "Platelet_count",
                   color = 'Platelet_count', palette = "grey", add = "median", xlab = "TPO (pg/ml)",
                   label.select = list(top.up = 200, 20, 10, 7, 5, 3, top.down = 1), xlim = c(0,1000))



dipt2 <- ggdensity(df6, x = "tpo", y = "..density..", combine = T, rug = TRUE, fill = "Platelet_count",
                   color = 'Platelet_count', palette = "lancet", add = "median", xlab = "TPO (pg/ml)",
                   label.select = list(top.up = 200, 20, 10, 7, 5, 3, top.down = 1), xlim = c(0, 1000))


dipt3 <- ggdensity(df6, x = "tpo", y = "..density..", combine = T, rug = TRUE, fill = "Platelet_count",
                   color = 'Platelet_count', palette = "jco", add = "median",  xlab = "TPO (pg/ml)",
                   label.select = list(top.up = 200, 20, 10, 7, 5, 3, top.down = 1), xlim = c(0, 1000))


dipt4 <- ggdensity(df6, x = "tpo", y = "..density..", combine = T, rug = TRUE, fill = "Platelet_count",
                   color = 'Platelet_count', palette = "npg", add = "median",  xlab = "TPO (pg/ml)",
                   label.select = list(top.up = 200, 20, 10, 7, 5, 3, top.down = 1), xlim = c(0, 1000))


## combinations


library(cowplot)
ggdraw() +
  draw_plot(dipt1 + theme(legend.justification = "bottom"), 0, 0, 1, 1) +
  draw_plot(g1 + 
              theme(legend.justification = "top"), 0.48, 0.4, 0.5, 0.48) +
  draw_plot_label(c("A.", "B."), c(0, 0.46), c(1, 0.86), size = 22)


ggdraw() +
  draw_plot(dipt2 + theme(legend.justification = "bottom"), 0, 0, 1, 1) +
  draw_plot(g1 + 
              theme(legend.justification = "top"), 0.48, 0.4, 0.5, 0.48) +
  draw_plot_label(c("A.", "B."), c(0, 0.46), c(1, 0.86), size = 22)

ggdraw() +
  draw_plot(dipt3 + theme(legend.justification = "bottom"), 0, 0, 1, 1) +
  draw_plot(g1 + 
              theme(legend.justification = "top"), 0.48, 0.4, 0.5, 0.48) +
  draw_plot_label(c("A.", "B."), c(0, 0.46), c(1, 0.86), size = 22)

ggdraw() +
  draw_plot(dipt4 + theme(legend.justification = "bottom"), 0, 0, 1, 1) +
  draw_plot(g1 + 
              theme(legend.justification = "top"), 0.48, 0.4, 0.5, 0.48) +
  draw_plot_label(c("A.", "B."), c(0, 0.46), c(1, 0.86), size = 22)








