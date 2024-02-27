set.seed(12345)

par(mar=rep(0.2, 4))

data_Matrix <- matrix(rnorm(400), nrow=40)


image(1:10, 1:40, t(data_Matrix)[,nrow(data_Matrix):1])

heatmap(data_Matrix)

set.seed(678910)
for (i in 1:40) {
  # coin_Flip <- rbinom(1, size=1, prob=0.5)
  if (rbinom(1, size=1, prob=0.5)) {
    data_Matrix[i,] <- data_Matrix[i,] + rep(c(0,3),each=5)
  }
}

image(1:10, 1:40, t(data_Matrix)[,nrow(data_Matrix):1])
heatmap(data_Matrix)


hh <- hclust(dist(data_Matrix))
data_Matrix_Ordered <- data_Matrix[hh$order,]
par(mfrow=c(1,3))
image(t(data_Matrix_Ordered)[, nrow(data_Matrix_Ordered):1])
plot(rowMeans(data_Matrix_Ordered), 40:1, xlab="Row Mean", ylab="Row", pch=19)
plot(colMeans(data_Matrix_Ordered), xlab="Column", ylab="Column Mean", pch=19)

# ======================================================================================
# =============================  Exercise 2  ===========================================
# ======================================================================================
# dev.off()


# library(dplyr)
# library(ggplot2)
library(tidyverse) #ggplot and dplyr
library(class) #knn

abalone <- read.csv("./abalone.csv", stringsAsFactors = TRUE)

abalone$Sex <- as.integer(abalone$Sex)

# data$iGender = as.integer(data$Gender)

ggplot(data=abalone,aes(x=Sex,fill=Sex))+geom_bar()

head(abalone)

summary(abalone)

attach(abalone)
abalone_matrix <- table(Sex, Rings)

# image(t(abalone_matrix)[,nrow(abalone_matrix):1], xlab="x", ylab="y")



cor(Length, Rings)
cor(Diameter, Rings)
cor(Height, Rings)
cor(Whole.weight, Rings)
cor(Shucked.weight, Rings)
cor(Viscera.weight, Rings)
cor(Shell.weight, Rings)

# ggplot(data=abalone,aes(x=Shell.weight,y=Rings,color=Sex))+geom_point()+geom_smooth(method="lm")

# shell weight has the highest correlation w/ the age, so I will use that
ggplot(data=abalone, aes(x=Shell.weight, y=Rings, color=Sex)) +
  geom_point() +
  geom_smooth(method="lm")

naba <- dim(abalone)[1]

sampling.rate <- 0.9

num.test.set.labels <- naba * (1.-sampling.rate)

training <- sample(1:naba, sampling.rate*naba, replace=FALSE)

train <- subset(abalone[training,], select=c("Sex", "Length", "Diameter", "Height", "Whole.weight", "Shucked.weight", "Viscera.weight", "Shell.weight"))

testing <- setdiff(1:naba, training)

test <- subset(abalone[testing,], select=c("Sex", "Length", "Diameter", "Height", "Whole.weight", "Shucked.weight", "Viscera.weight", "Shell.weight"))

crings <- Rings[training]

true.labels <- Rings[testing]

classif <- knn(train, test, crings, k=6)  # (main.R#90): NA/NaN/Inf in foreign function call (arg 6)

classif

attributes(.Last.value)

# ======================================================================================
# =============================  Exercise 3  ===========================================
# ======================================================================================

# Create a new data frame and remove the fifth
# column

df <- iris[,-5] # without 5th col
df

# Apply kmeans (you choose k) with 1000
# iterations

set.seed(101)
help(kmeans)
irisClusters <- kmeans(df, 3, iter.max = 1000)

clusplot(iris,irisClusters$cluster, color = TRUE, shade = TRUE, labels = 0, lines = 0)
par(pty="m")
table(iris[,5], irisClusters$cluster)


# ======================================================================================
# ===============================  Titanic  ============================================
# ======================================================================================
library(titanic)
library(tidyverse)
head(titanic_train)
titanic <- titanic_train
str(titanic_train)
summary(titanic_train)

help(rpart)
help(ctree)
help(hclust)

any(is.na(titanic_train))
titanic <- na.omit(titanic) # remove columns with na values
