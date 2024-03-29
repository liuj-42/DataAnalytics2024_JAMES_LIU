
data("iris")
head(iris)

irisdata1 <- iris[,1:4]
irisdata1

help(princomp)
principal_components <- princomp(irisdata1, cor=T, score=T)
summary(principal_components)

plot(principal_components)
plot(principal_components, type="l")

help(biplot)
biplot(principal_components)

# install.packages("MASS")
library(MASS)
data(Boston, package="MASS")

help(Boston)
help(prcomp)
pca_out <- prcomp(Boston, scale=T)
pca_out
plot(pca_out)

help(biplot)

biplot(pca_out, scale=0)
boston_pc <- pca_out$x
boston_pc
head(boston_pc)
summary(boston_pc)

data("USArrests")
help("USArrests")

states <- row.names(USArrests)
states

names(USArrests)

apply(USArrests, 2, mean)
apply(USArrests, 2, var)

pr.out <- prcomp(USArrests, scale=T)
names(pr.out)

pr.out$center
pr.out$scale

pr.out$rotation

dim(pr.out$x)

biplot(pr.out, scale=0)

pr.out$sdev
pr.var <- pr.out$sdev^2
pr.var

pve <- pr.var/sum(pr.var)
pve

data(Titanic)

# randomforest

library(tidyverse)

