library(e1071)
set.seed(1)
x <- matrix(rnorm(20 * 2), ncol = 2)
y <- c(rep(-1, 10), rep(1,10))
x[y == 1,]<- x[y==1,] + 1
plot(x, col = (3-y))

data <- data.frame(x = x, y = as.factor(y))
model_svm <- svm(y ~ ., data = data, kernel = "linear", cost = 10, scale = FALSE)
plot(model_svm, data)
model_svm$index
summary(model_svm)

model_svm <- svm(y ~ ., data = data, kernel = "linear", cost = 0.1, scale = FALSE)
plot(model_svm, data)
model_svm$index

model_tune <- tune(svm, y ~ ., data= data, kernel = "linear", 
                   ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5 ,10, 100)))
summary(model_tune)

best_model <- model_tune$best.model
summary(best_model)

xtest <- matrix(rnorm(20*2), ncol = 2)
ytest <- sample(c(-1,1), 20, rep = T)
xtest[ytest == 1,]= xtest[ytest == 1,] + 1
testdat <- data.frame(x = xtest, y = as.factor(ytest))
ypred <- predict(best_model, testdat)
table(predict = ypred, truth = testdat$y)

model_svm <- svm(y ~ ., data = data, kernel = "linear", cost = 0.01, scale = F)
ypred <- predict(model_svm, testdat)
table(predict = ypred, truth = testdat$y)

x[y==1, ] = x[y==1, ]+ 0.5
plot(x, col = (y + 5)/2, pch = 19)
data <- data.frame(x = x, y = as.factor(y))
model_svm <- svm(y ~ ., data = data, kernel = "linear", cost = 1e5)
summary(model_svm)
plot(model_svm, data)

model_svm <- svm(y ~ ., data = data, kernel = "linear", cost = 1)
summary(model_svm)
plot(model_svm, data)

x <- matrix(rnorm(200 * 2), ncol = 2)
x[1:100,] <- x[1:100, ] + 2
x[101:150, ] <- x[101:150, ] -2
y <- c(rep(1, 150), rep(2,50))
data = data.frame(x = x, y = as.factor(y))
plot(x , col = y)

train <- sample(200,100)
model_svm <- svm(y ~ ., data = data[train,], kernel = "radial", gamma = 1, cost = 1)
plot(model_svm, data[train,])
summary(model_svm)

model_svm <- svm(y ~ ., data = data[train,], kernel = "radial", gamma = 1, cost = 1e5)
plot(model_svm, data[train,])

model_tune <- tune(svm, y~., data = data[train,], kernel = "radial", 
                   ranges = list(cost = c(0.1, 1, 10, 100, 1000),
                   gamma = c(0.5, 1, 2, 3, 4)))
summary(model_tune)
table(true = data[-train, "y"], pred= predict(model_tune$best.model, newdata = data[-train,]))
(1 + 3)/(70 + 3 + 1 + 26)

library(ROCR)
rocplot = function(pred, truth, ...){
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  plot(perf, ...)
}

model_svm <- svm(y ~ ., data = data[train,], kernel = "radial", gamma = 2, cost = 1, 
                 decision.values = T)
fitted <- attributes(predict(model_svm, data[train,],decision.values = T))$decision.values
par(mfrow = c(1,2))
rocplot(fitted, data[train, "y"], main = "Training Data")

svm_flex <- svm(y ~ ., data = data[train,], kernel = "radial", gamma = 50, cost = 1, 
                decison.values = T)
fitted <- attributes(predict(svm_flex, data[train,],decision.values = T))$decision.values
rocplot(fitted, data[train, "y"], add = T, col = "red")
summary(svm_flex)

fitted <- attributes(predict(model_svm, data[-train,],decision.values = T))$decision.values
rocplot(fitted, data[-train, "y"], main = "Test Data")
fitted <- fitted <- attributes(predict(svm_flex, data[-train,],decision.values = T))$decision.values
rocplot(fitted, data[-train, "y"], add = T, col = "red")

set.seed(1)
x <- rbind(x, matrix(rnorm(50 * 2), ncol = 2))
y <- c(y, rep(0, 50))
x[y == 0, 2] <- x[y == 0,2] + 2
data = data.frame(x = x, y = as.factor(y))
par(mfrow = c(1,1))
plot(x, col = (y + 1))

model_svm <- svm(y ~ ., data = data, kernel = "radial", cost = 10, gamma = 1)
plot(model_svm, data)

library(ISLR)
names(Khan)
table(Khan$ytrain)
table(Khan$ytest)
data = data.frame(x = Khan$xtrain, y = as.factor(Khan$ytrain))
out <- svm(y ~ ., data = data, kernel = "linear", cost = 10)
summary(out)

table(out$fitted, data$y)

test_data <- data.frame(x = Khan$xtest, y =as.factor(Khan$ytest))
pred <- predict(out, newdata = test_data)
table(pred, test_data$y)

set.seed(131)
X <- rnorm(100)
Y <- 3 * X^2 + 4 + rnorm(100)
train <- sample(100, 50)
Y[train] <- Y[train] + 3
Y[-train] <- Y[-train] - 3
plot(X[train], Y[train], col = "red", ylim=c(-4, 20))
points(X[-train], Y[-train], col = "blue")

set.seed(315)
Z <- rep(0,100)
Z[train] <- 1
final_train <- c(sample(train, 25), sample(setdiff(1:100, train), 25))
train_data <- data.frame(x=X[final_train], y=Y[final_train], z=as.factor(Z[final_train]))
test_data <- data.frame(x=X[-final_train], y=Y[-final_train], z=as.factor(Z[-final_train]))
library(e1071)

svm_linear <- svm(z~., data=train_data, kernel="linear", cost=10)
plot(svm_linear, train_data)
table(Z[final_train], predict(svm_linear, train_data))

set.seed(32545)
svm_poly <- svm(z~., data=train_data, kernel="polynomial", cost=10)
plot(svm_poly, train_data)
table(Z[final_train], predict(svm_poly, train_data))

set.seed(996)
svm_radial <- svm(z~., data=train_data, kernel="radial", gamma=1, cost=10)
plot(svm_radial, train_data)
table(Z[final_train], predict(svm_radial, train_data))

plot(svm_linear, test_data)
plot(svm_poly, test_data)
plot(svm_radial, test_data)

table(Z[-final_train], predict(svm_linear, test_data))
table(Z[-final_train], predict(svm_poly, test_data))
table(Z[-final_train], predict(svm_radial, test_data))

set.seed(421)
x1 <- runif(500) - 0.5
x2 <- runif(500) - 0.5
y <- 1 * (x1 ^ 2 - x2 ^ 2 > 0.05)

plot(x1[y ==0], x2[y==0], col = "red", pch = "+")
points(x1[y == 1], x2[y == 1], col = "blue", pch = 4)

model_lm <- glm(y ~ x1 + x2, family = binomial)
summary(model_lm)

data <- data.frame(x1 = x1, x2 = x2, y = y)
prob <- predict(model_lm, data, type = "response")
pred <- ifelse(prob > 0.33, 1, 0)
pos <- data[pred == 1,]
neg <- data[pred == 0,]
plot(pos$x1, pos$x2, col = "blue")
points(neg$x1, neg$x2, col = "red")

model_lm <- glm(y ~ poly(x1, 2) + poly(x2, 2) + I(x1 * x2), data = data, family = binomial)
prob <- predict(model_lm, data, type = "response")
pred <- ifelse(prob > 0.33, 1, 0)
pos <- data[pred == 1,]
neg <- data[pred == 0,]
plot(pos$x1, pos$x2, col = "blue")
points(neg$x1, neg$x2, col = "red")

library(e1071)
model_svm <- svm(as.factor(y) ~ x1 + x2, data, kernel = "linear", cost = 10)
pred_svm <- predict(model_svm, data)
pos <- data[pred_svm == 1,]
neg <- data[pred_svm == 0,]
plot(pos$x1, pos$x2, col = "blue")
plot(neg$x1, neg$x2)

model_svm <- svm(as.factor(y) ~ x1 + x2, data, gamma = 1)
pred_svm <- predict(model_svm, data)
pos <- data[pred_svm == 1,]
neg <- data[pred_svm == 0,]
plot(pos$x1, pos$x2, col = "blue")
points(neg$x1, neg$x2, col = "red")

set.seed(3154)
X_one <- runif(500, 0, 90)
Y_one <- runif(500, X_one + 10, 100)
X_one_noise <- runif(50, 20, 100)
Y_one_noise <- 5/4 * (X_one_noise - 10) + 0.1

X_zero <- runif(500, 10, 100)
Y_zero <- runif(500, 0, X_zero - 10)
X_zero_noise <- runif(50, 20, 80) 
Y_zero_noise <- 5/4 * (X_zero_noise - 10) - 0.1

class_one <- seq(1, 550)
X <- c(X_one, X_one_noise, X_zero, X_zero_noise)
Y <- c(Y_one, Y_one_noise, Y_zero, Y_zero_noise)
plot(X[class_one], Y[class_one], col = "blue")
points(X[-class_one], Y[-class_one], col = "red")

library(e1071)
set.seed(555)
Z <- rep(0,1100)
Z[class_one] <- 1
data <- data.frame(x = X, y = Y, z = Z)
model_tune <- tune(svm, as.factor(z) ~ ., data = data, kernel = "linear", 
                   ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100, 1000, 10000)))
summary(model_tune)
data.frame(cost = model_tune$performances$cost, misclass = model_tune$performances$error * 1100)

set.seed(1111)
X_test <- runif(1000, 0, 100)
test_class_one <- sample(1000, 500)
Y_test <- rep(NA, 1000)
for(i in test_class_one){
  Y_test[i] <- runif(1, X_test[i], 100)
}
for(i in setdiff(1:1000, test_class_one)){
  Y_test[i] <- runif(1, 0, X_test[i])
}
plot(X_test[test_class_one], Y_test[test_class_one], col = "blue")
points(X_test[-test_class_one], Y_test[-test_class_one], col = "red")

set.seed(30012)
Z_test <- rep(0,1000)
Z_test[test_class_one] <- 1
costs <- c(0.01, 0.1, 1, 5, 10, 100, 1000, 10000)
test_errors <- rep(NA, 8)
test_data <- data.frame(x = X_test, y = Y_test, z = Z_test)
for(i in 1 : length(costs)){
  model_svm <- svm(as.factor(z) ~ ., data = data, kernel = "linear", cost = costs[i])
  pred <- predict(model_svm, test_data)
  test_errors[i] <- sum(pred != test_data$z)
}
data.frame(cost = costs, misclass = test_errors)

library(ISLR)
gas_median <- median(Auto$mpg)
var <- ifelse(Auto$mpg > gas_median, 1, 0)
Auto$mpglevel <- as.factor(var)

set.seed(3255)
model_tune <- tune(svm, mpglevel ~ ., data = Auto, kernel = "linear", 
                   ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100)))
summary(model_tune)

model_tune <- tune(svm, mpglevel ~ ., data = Auto, kernel = "polynomial", 
                   ranges = list(cost = c(0.1, 1, 5, 10), degree = c(2,3,4)))
summary(model_tune)

model_tune <- tune(svm, mpglevel ~ ., data = Auto, kernel = "radial", 
                   ranges = list(cost = c(0.1, 1, 5, 10), gamma = c(0.01, 0.1, 1, 5, 10, 100)))
summary(model_tune)

svm_linear <- svm(mpglevel ~ ., data = Auto, kernel = "linear",cost = 1)
svm_poly <- svm(mpglevel ~ ., data = Auto, kernel = "polynomial", cost = 10, degree = 2)
svm_radial <- svm(mpglevel ~ ., data = Auto, kernel = "radial", gamma = 0.01)

plotpairs <- function(model){
  for(name in names(Auto)[!names(Auto) %in%c("mpg", "mpglevel", "name")]){
    plot(model, Auto, as.formula(paste("mpg~", name, sep = "")))
  }
}

plotpairs(svm_linear)
plotpairs(svm_poly)
plotpairs(svm_radial)

library(ISLR)
set.seed(9004)
train <- sample(dim(OJ)[1], 800)
train_data <- OJ[train,]
test_data <- OJ[-train,]
library(e1071)
svm_linear <- svm(Purchase ~ ., kernel = "linear", data = train_data, cost= 0.01)
summary(svm_linear)

train_pred <- predict(svm_linear, train_data)
table(train_data$Purchase, train_pred)
(76 + 61)/(418 + 76+ 61 + 245)
test_pred <- predict(svm_linear, test_data)
table(test_data$Purchase, test_pred)
(17 + 27)/(157 + 17 + 27 + 69)

model_tune <- tune(svm, Purchase ~ ., data= train_data, kernel = "linear", 
                   ranges = list(cost = 10^seq(-2, 1, by = 0.25)))
summary(model_tune)

model_linear_best <- svm(Purchase ~ ., kernel = "linear", data = train_data, 
                         cost= model_tune$best.parameters$cost)
train_best_pred <- predict(model_linear_best, train_data)
table(train_data$Purchase, train_best_pred)
(65 + 70)/(414 + 65 + 70 + 251)

test_best_pred <- predict(model_linear_best, test_data)
table(test_data$Purchase, test_best_pred)
(16 + 24) / (158 + 16 + 24 + 72)

svm_radial <- svm(Purchase ~ ., data = train_data, kernel = "radial")
summary(svm_radial)

train_pred <- predict(svm_radial, train_data)
table(train_data$Purchase, train_pred)
(57 + 69) / (422 + 69 + 57 + 252)
test_pred <- predict(svm_radial, test_data)
table(test_data$Purchase, test_pred)
(13 + 25) / (161 + 25 + 13 + 71)

model_tune_radial <- tune(svm, Purchase ~ ., data = train_data, kernel = "radial",
                          ranges = list(cost = 10^seq(-2, 1, by = 0.25)))
summary(model_tune_radial)

svm_radial_best <- svm(Purchase ~ ., data = train_data, kernel = "radial",
                       cost= model_tune_radial$best.parameters$cost)
summary(svm_radial_best)
train_best_pred <- predict(svm_radial_best, train_data)
table(train_data$Purchase, train_best_pred)
(67 + 52)/(427 + 67 + 52 + 254)
test_best_pred <- predict(svm_radial_best, test_data)
table(test_data$Purchase, test_best_pred)
(13 + 29) / (161 + 29 + 13 + 67)

svm_poly <- svm(Purchase ~ ., data = train_data, kernel = "poly", degree = 2)
summary(svm_poly)

train_pred <- predict(svm_poly, train_data)
table(train_data$Purchase, train_pred)
(114 + 36) / (443 + 114 + 36 + 207)

test_pred <- predict(svm_poly, test_data)
table(test_data$Purchase, test_pred)
(10 + 34) / (164 + 34 + 10 + 62)

model_tune_poly <- tune(svm, Purchase ~ ., data = train_data, kernel = "poly",
                       ranges = list(cost = 10^seq(-2, 1, by = 0.25)))
summary(model_tune_poly)

svm_poly <- svm(Purchase ~ ., data = train_data, kernel = "poly", degree = 2,
                cost = model_tune_poly$best.parameters$cost)
train_pred <- predict(svm_poly, train_data)
table(train_data$Purchase, train_pred)
(43 + 79) / (436 + 79 + 43 + 242)

test_pred <- predict(svm_poly, test_data)
table(test_data$Purchase, test_pred)
(11 + 33) / (163 + 33 + 11 + 63)
