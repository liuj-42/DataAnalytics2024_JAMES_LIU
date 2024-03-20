

cars1 <- cars[1:30,]
head(cars1)

cars_outliers <- data.frame(speed=c(19,19,20,20,20), dist=c(190,186,210,220,218))
head(cars_outliers)

cars2 <- rbind(cars1, cars_outliers)

help(par)

par(mfrow=c(1,2))

plot(cars2$speed, cars2$dist, xlim=c(0,28), ylim=c(0,230), main="With Outliers", xlab="speed", ylab="dist", pch="*", col="red", cex=2)

abline(lm(dist ~ speed, data=cars2), col="blue", lwd=3, lty=2)

plot(cars1$speed, cars1$dist, xlim=c(0, 28), ylim=c(0, 230), main="Outliers removed \nA much better fit!", xlab="speed", ylab="dist", pch="*", col="red", cex=2)

abline(lm(dist~speed, data=cars1), col="blue", lwd=3, lty=2)


# =====================================================================
# =====================================================================
# knn & kmeans examples
# ======================================================================
# ======================================================================
# knn

library(ISLR)
library(class)
head(Caravan)
str(Caravan)

summary(Caravan)
summary(Caravan$Purchase)

any(is.na(Caravan))

var(Caravan[,1])
var(Caravan[,2])
var(Caravan[,3])

purchase <- Caravan[,86]

purchase

StandarizedCaravan <- scale(Caravan[, -86])

var(StandarizedCaravan[,1])
var(StandarizedCaravan[,2])
var(StandarizedCaravan[,3])

test_index <- 1:1000
test_data <- StandarizedCaravan[test_index,]
test_purchase <- purchase[test_index]

train_data <- StandarizedCaravan[-test_index,]
train_purchase <- purchase[-test_index]

set.seed(101)
predicted_purchase <- knn(train_data, test_data, train_purchase, k=10)
head(predicted_purchase)

missClassError <- mean(test_purchase != predicted_purchase)
print(missClassError)

predicted_purchase <- NULL
error_rate <- NULL

for (i in 1:20) {
  set.seed(101)
  predicted_purchase <- knn(train_data, test_data, train_purchase, k=i)
  error_rate[i] <- mean(test_purchase != predicted_purchase)
}

print(error_rate)
library(ggplot2)

k_values <- 1:20

error_df <- data.frame(error_rate, k_values)
print(error_df)

ggplot(error_df, aes(k_values, error_rate)) + geom_point() + geom_line(lty="dotted", color="blue")


# kmeans
head(iris)
str(iris)

plot1 <- ggplot(iris, aes(Petal.Length, Petal.Width, color=Species))

print(plot1 + geom_point(size=3))



set.seed(101)

help(kmeans)

irisClusters <- kmeans(iris[,1:4], 3, nstart=20)

print(irisClusters)
table(irisClusters$cluster, iris$Species)

library(cluster)
help(clusplot)
clusplot(iris, irisClusters$cluster, color=T, shade=T, labels=0, lines=0)