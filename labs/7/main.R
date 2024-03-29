
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
library(titanic)
library(randomForest)

# randomforest

library(tidyverse)
library(rpart)

head(titanic_train)

summary(titanic_train)
summary(titanic_train$Survived)

# fit <- rpart(Survived~., data=titanic_train[!is.na(titanic_train$Age),], method="anova")

fit <- randomForest(Survived~., data=titanic_train[!is.na(titanic_train$Age),])
# ntree restricts the number of trees generated
# nodesize reduces the complexity of each tree generated
# samsize reduces the number of rows sampled


Prediction <- predict(fit, titanic_test)
results <- data.frame(PassengerId = titanic_test$PassengerId, Survived = Prediction)

library(party)

# Convert all character columns to factors
titanic_train[] <- lapply(titanic_train, function(x) {
  if (is.character(x)) as.factor(x) else x
})

# Now you can run the cforest function
fit2 <- cforest(Survived~., data=titanic_train[!is.na(titanic_train$Age),])

