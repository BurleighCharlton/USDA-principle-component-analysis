######Clean data
inputdata = read.csv("pca_data.csv")
inputdata = na.omit(inputdata) # remove rows with missing values
inputdata = inputdata[,-1]

data = inputdata[, -c(1:6)] # restrict to just the usefull collumns

str(data)


###
### libraries
###

library(reshape2)
library(ggplot2)


### PCA


pr.out = prcomp(data, center=TRUE, scale=TRUE)
pr.out$x = -pr.out$x
pr.out$rotation = -pr.out$rotation

pve = pr.out$sdev^2 / length(pr.out$sdev)  # Proportion of variance explained

library(ggplot2)
library(gridExtra)

# prop var explained and cumulative

p1 = ggplot() +
  geom_line(aes(x=c(1:length(pr.out$sdev)), y=pve)) +
  geom_point(aes(x=c(1:length(pr.out$sdev)), y=pve), size=3) +
  geom_hline(yintercept=pve[7], color='red') +
  labs(x="Principal component", y="Proportion of variance explained")

p2 = ggplot() +
  geom_line(aes(x=c(1:length(pr.out$sdev)), y=cumsum(pve))) +
  geom_point(aes(x=c(1:length(pr.out$sdev)), y=cumsum(pve)), size=3) +
  geom_hline(yintercept=cumsum(pve)[7], color='red') +
  annotate("text", x=20, y=0.59, label=paste(round(cumsum(pve)[7],3))) +
  labs(x="Principal component", y="Cumulative proportion of variance explained")

grid.arrange(p1, p2, ncol=2)

#correlation
library(corrplot)
data.cor = cor(data)
corrplot(data.cor, method="square", type="upper")