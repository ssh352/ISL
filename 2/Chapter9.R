set.seed(1)
x <- matrix(rnorm(20 * 2), ncol = 2)
y <- c(rep(-1, 10), rep(1,10))
x[y == 1,] <- x[y==1,]+1
plot(x, col = (3 - y))

dat <- data.frame(x = x, y = as.factor(y))
library(e1071)
model_svm <- svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = F)
plot(model_svm, dat)
model_svm$index
summary(model_svm)

model_svm <- svm(y ~ ., data = dat, kernel = "linear", cost = 0.01, scale = F)
plot(model_svm, dat)
model_svm$index
summary(model_svm)

tune_out <- tune(svm, y ~ ., data = dat, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune_out)
best_model <- tune_out$best.model
summary(best_model)

xtest <- matrix(rnorm(20 * 2), ncol = 2)
ytest <- sample(c(-1, 1), 20, rep = T)
xtest[ytest == 1,] <- xtest[ytest == 1,] + 1
testdat <- data.frame(x = xtest, y = as.factor(ytest))
ypred <- predict(best_model, testdat)
table(predict = ypred, truth = testdat$y)

model_svm <- svm(y ~ ., data = dat, kernel = "linear", cost = 0.01, scale = F)
ypred <- predict(model_svm, testdat)
table(predict = ypred, truth = testdat$y)

x[y == 1,] <- x[y==1, ] + 0.5
plot(x, col = (y + 5)/2, pch = 19)

dat <- data.frame(x = x, y = as.factor(y))
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 1e5)
summary(svmfit)
plot(svmfit, dat)

svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 1)
summary(svmfit)
plot(svmfit, dat)

set.seed(1)
x <- matrix(rnorm(200 * 2), ncol = 2)
x[1:100,] <- x[1:100,] + 2
x[101:150, ] <- x[101:150,]-2
y <- c(rep(1, 150), rep(2, 50))
dat <- data.frame(x = x, y = as.factor(y))

plot(x, col = y)

train <- sample(200, 100)
svmfit <- svm(y ~ ., data = dat[train,], kernel = "radial", gamma = 1, cost = 1)
plot(svmfit, dat[train,])
summary(svmfit)

svmfit <- svm(y ~ ., data = dat[train,], kernel = "radial", gamma = 1, cost = 1e5)
summary(svmfit)
plot(svmfit, dat[train,])

set.seed(1)
tune.out <- tune(svm, y ~ ., data = dat[train,], kernel = "radial",
                 ranges = list(cost = c(0.1, 1, 10, 100, 1000),
                 gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out)
table(true = dat[-train, "y"], pred = predict(tune.out$best.model, newdata = dat[-train,]))
(10 + 2) / (67 + 10 + 2 + 21)

library(ROCR)
rocplot <- function(pred, truth, ...){
  predob <- prediction(pred, truth)
  pref <- performance(predob, "tpr", "fpr")
  plot(pref, ...)
}

svmfit_opt <- svm(y ~ ., data = dat[train,], kernel = "radial", gamma = 2, 
                  cost = 1, decision.values = T)
fitted <- attributes(predict(svmfit_opt, dat[train,], decision.values = T))$decision.values
par(mfrow = c(1,2))
rocplot(fitted, dat[train, "y"])

svmfit_opt <- svm(y ~ ., data = dat[train,], kernel = "radial", gamma = 50, 
                  cost = 1, decision.values = T)
fitted <- attributes(predict(svmfit_opt, dat[train,], decision.values = T))$decision.values
rocplot(fitted, dat[train, "y"])

svmfit_opt <- svm(y ~ ., data = dat[train,], kernel = "radial", gamma = 2, 
                  cost = 1, decision.values = T)
fitted <- attributes(predict(svmfit_opt, dat[-train,], decision.values = T))$decision.values
rocplot(fitted, dat[train, "y"])


svmfit_opt <- svm(y ~ ., data = dat[train,], kernel = "radial", gamma = 50, 
                  cost = 1, decision.values = T)
fitted <- attributes(predict(svmfit_opt, dat[-train,], decision.values = T))$decision.values
rocplot(fitted, dat[train, "y"])

library(ISLR)
names(Khan)
attach(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
table(Khan$ytrain)
table(Khan$ytest)

dat <- data.frame(x = xtrain, y = as.factor(ytrain))
out <- svm(y ~ ., data = dat, kernel = "linear", cost = 10)
summary(out)
table(out$fitted, dat$y)

dat_test <- data.frame(x = Khan$xtest, y = as.factor(ytest))
pred_test <- predict(out, newdata = dat_test)
table(pred_test, dat_test$y)

set.seed(131)
x <- rnorm(100)
y <- 3 * x^2 + 4 + rnorm(100)
train <- sample(100, 50)
y[train] <- y[train] + 3
y[-train] <- y[-train] - 3
plot(x[train], y[train], col = "red", ylim = c(-4,18))
points(x[-train], y[-train], col = "blue")

set.seed(315)
z <- rep(0, 100)
z[train] <- 1

final_train <- c(sample(train, 25), sample(setdiff(1:100, train), 25))
train_data <- data.frame(x = x[final_train], y = y[final_train], z = as.factor(z[final_train]))
test_data <- data.frame(x = x[-final_train], y = y[-final_train], z = as.factor(z[-final_train]))

library(e1071)

svm_linear <- svm(z ~ ., data = train_data, kernel = "linear", cost = 10)
plot(svm_linear, train_data)
table(z[final_train], predict(svm_linear, train_data))

set.seed(32545)
svm_poly <- svm(z ~ ., data = train_data, kernel = "polynomial", cost = 10)
plot(svm_poly, train_data)
table(z[final_train], predict(svm_poly, train_data))

set.seed(996)
svm_radial <- svm(z ~ ., data = train_data, kernel = "radial", gamma = 1, cost = 10)
plot(svm_radial, train_data)
table(z[final_train], predict(svm_radial, train_data))

plot(svm_linear, test_data)
plot(svm_poly, test_data)
plot(svm_radial, test_data)

table(z[-final_train], predict(svm_linear, train_data))
table(z[-final_train], predict(svm_poly, train_data))
table(z[-final_train], predict(svm_radial, train_data))

set.seed(1)
x1 <- runif(500) - 0.5
x2 <- runif(500) - 0.5
y <- 1 * (x1 ^ 2 - x2 ^ 2 > 0.05)
plot(x1[y ==0], x2[y==0], col = "red")
points(x1[y ==1], x2[y==1], col = "blue")

model_logi <- glm(y ~ x1 + x2, family = binomial)
summary(model_logi)

data <- data.frame(x1 = x1, x2 = x2, y =y)
logi_prob <- predict(model_logi, data, type = "response")
logi_pred <- ifelse(logi_prob > 0.32, 1, 0)
data_pos <- data[logi_pred == 1,]
data_neg <- data[logi_pred == 0,]
plot(data_pos$x1, data_pos$x2, col = "blue", xlim = c(-0.5, 0.6))
points(data_neg$x1, data_neg$x2, col = "red")

model_logi2 <- glm(y ~ poly(x1, 2) + poly(x2, 2) + I(x1 * x2), data = data, family = binomial)
logi_prob2 <- predict(model_logi2, data, type = "response")
logi_pred2 <- ifelse(logi_prob2 > 0.5, 1, 0)
data_pos2 <- data[logi_pred2 == 1,]
data_neg2 <- data[logi_pred2 == 0,]
plot(data_pos2$x1, data_pos2$x2, col = "blue")
points(data_neg2$x1, data_neg2$x2, col = "red")

library(e1071)

svm_linear <- svm(as.factor(y) ~ x1 + x2, data, kernel = "linear", cost = 0.1)
svm_pred <- predict(svm_linear, data)
data_pos_linear <- data[svm_pred==1,]
data_neg_linear <- data[svm_pred==0, ]
plot(data_pos_linear$x1, data_pos_linear$x2, col = "blue", xlim = c(-1.0, 0.6), ylim = c(-1,1))
points(data_neg_linear$x1, data_neg_linear$x2, col = "red")

svm_poly <- svm(as.factor(y) ~ x1 + x2, data, gamma = 1)
svm_pred <- predict(svm_poly, data)
data_pos_poly <- data[svm_pred==1,]
data_neg_poly <- data[svm_pred==0, ]
plot(data_pos_poly$x1, data_pos_poly$x2, col = "blue")
points(data_neg_poly$x1, data_neg_poly$x2, col = "red")

set.seed(3154)
x_one <- runif(500, 0, 90)
y_one <- runif(500, x_one + 10, 100)
x_one_noise <- runif(50, 20, 80)
y_one_noise <- 5/4 * (x_one_noise - 10) + 0.1

x_zero <- runif(500, 10, 100)
y_zero <- runif(500, 0, x_zero - 10)
x_zero_noise <- runif(50, 20, 80)
y_zero_noise <- 5/4 * (x_zero_noise - 10) + 0.1

class_one <- seq(1, 550)
x <- c(x_one, x_one_noise, x_zero, x_zero_noise)
y <- c(y_one, y_one_noise, y_zero, y_zero_noise) 
plot(x[class_one], y[class_one], col = "blue", pch = "+", ylim = c(0, 100))
points(x[-class_one], y[-class_one], col = "red", pch = 4)

library(e1071)
set.seed(555)
z <- rep(0, 1100)
z[class_one] <- 1
data <- data.frame(x = x, y = y, z = z)
tune_out <- tune(svm, as.factor(z) ~ ., data = data, kernel = "linear",
                 ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100, 10000)))
summary(tune_out)
data.frame(cost = tune_out$performances$cost, missclass = tune_out$performances$error * 1100)

set.seed(1111)
test_x <- runif(1000, 0, 100)
class_one <- sample(1000, 500)
y_test <- rep(NA, 1000)

for(i in class_one){
  y_test[i] <- runif(1, test_x[i], 100)
}

for (i in setdiff(1:1000, class_one)) {
  y_test[i] <- runif(1, 0, test_x[i])
}
plot(test_x[class_one], y_test[class_one], col = "blue", pch = "+")
points(test_x[-class_one], y_test[-class_one], col = "red", pch = 4)

set.seed(30012)
z_test <- rep(0, 1000)
z_test[class_one] <- 1
all_costs <- c(0.01, 0.1, 1, 5, 10, 100, 1000, 10000)
test_errors <- rep(NA, 8)
data_test <- data.frame(x = test_x, y = y_test, z = z_test)
for (i in 1:length(all_costs)) {
  svm_fit <- svm(as.factor(z) ~ ., data = data, kernel = "linear", cost = all_costs[i])
  svm_predict = predict(svm_fit, data_test)
  test_errors[i] = sum(svm_predict != data_test$z)
}

data.frame(cost = all_costs, `test misclass` = test_errors)

library(ISLR)
attach(Auto)
gas_median <- median(mpg)
new_var <- ifelse(mpg > gas_median, 1, 0)
Auto$mpglevel <- as.factor(new_var)

tune_out <- tune(svm, mpglevel ~ ., data = Auto, kernel = "linear", 
                 ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100)))
summary(tune_out)

tune_poly <- tune(svm, mpglevel ~ ., data = Auto, kernel = "polynomial", 
                  ranges = list(cost = c(0.01, 0.1, 1, 5, 10), degree = c(2, 3, 4)))
summary(tune_poly)                  

tune_rad <- tune(svm, mpglevel ~ ., data = Auto, kernel = "radial", 
                  ranges = list(cost = c(0.01, 0.1, 1, 5, 10), gamma = c(0.01, 0.1, 1, 5, 10, 100)))
summary(tune_rad) 


svm_linear = svm(mpglevel ~ ., data = Auto, kernel = "linear", cost = 1)
svm_poly = svm(mpglevel ~ ., data = Auto, kernel = "polynomial", cost = 10, 
               degree = 2)
svm_radial = svm(mpglevel ~ ., data = Auto, kernel = "radial", cost = 10, gamma = 0.01)
plotpairs = function(fit) {
  for (name in names(Auto)[!(names(Auto) %in% c("mpg", "mpglevel", "name"))]) {
    plot(fit, Auto, as.formula(paste("mpg~", name, sep = "")))
  }
}
plotpairs(svm_linear)
plotpairs(svm_poly)
plotpairs(svm_radial)

library(ISLR)
attach(OJ)
set.seedI(9004)
train <- sample(dim(OJ)[1], 800)
train_data <- OJ[train,]
test_data <- OJ[-train,]

library(e1071)
svm_linear <- svm(Purchase ~ ., kernel = "linear", data = train_data, cost = 0.01)
summary(svm_linear)

train_pred <- predict(svm_linear, train_data)
table(train_data$Purchase, train_pred)
(58 + 82)/(433 + 58 + 82 + 227)

test_pred <- predict(svm_linear, test_data)
table(test_data$Purchase, test_pred)
(15 + 25)/(147 + 15 + 25 + 83)

set.seed(1554)
tune_out <- tune(svm, Purchase ~ ., data = train_data, kernal = "linear",
                 ranges = list(cost = 10 ^ seq(-2, 1, by = 0.25)))
summary(tune_out)

svm_linear_best <- svm(Purchase ~ ., kernel = "linear", data = train_data, 
                       cost = tune_out$best.parameters$cost)
train_pred <- predict(svm_linear, train_data)
table(train_data$Purchase, train_pred)
(58 + 82) / (433 + 58 + 82 + 227)

test_pred <- predict(svm_linear_best, test_data)
table(test_data$Purchase, test_pred)
(15 + 25) / (147 + 15 + 25 + 83)

svm_radial <- svm(Purchase ~ ., kernel = "radial", data = train_data)
summary(svm_radial)

train_pred <- predict(svm_radial, train_data)
table(train_data$Purchase, train_pred)
(38 + 82)/(453 + 38 + 81 + 228)

test_pred <- predict(svm_radial, test_data)
table(test_data$Purchase, test_pred)
(18 + 26)/(144 + 18 + 26 + 82)

set.seed(1554)
tune_out <- tune(svm, Purchase ~ ., data = train_data, kernal = "radial",
                 ranges = list(cost = 10 ^ seq(-2, 1, by = 0.25)))
summary(tune_out)

svm_radial_best <- svm(Purchase ~ ., kernel = "radial", data = train_data, 
                       cost = tune_out$best.parameters$cost)
train_pred <- predict(svm_radial_best, train_data)
table(train_data$Purchase, train_pred)
(41 + 82) / (450 + 41 + 82 + 227)

test_pred <- predict(svm_radial_best, test_data)
table(test_data$Purchase, test_pred)
(17 + 26) / (145 + 17 + 26 + 82)

svm_poly <- svm(Purchase ~ ., kernel = "poly", data = train_data, degree = 2)
summary(svm_poly)

train_pred <- predict(svm_poly, train_data)
table(train_data$Purchase, train_pred)
(32 + 117)/(459 + 32 + 117 + 192)

test_pred <- predict(svm_poly, test_data)
table(test_data$Purchase, test_pred)
(13 + 32)/(149 + 13 + 32 + 76)

set.seed(1554)
tune_out <- tune(svm, Purchase ~ ., data = train_data, kernal = "poly", degree = 2,
                 ranges = list(cost = 10 ^ seq(-2, 1, by = 0.25)))
summary(tune_out)

svm_poly_best <- svm(Purchase ~ ., kernel = "poly", data = train_data, 
                     cost = tune_out$best.parameters$cost)
train_pred <- predict(svm_poly_best, train_data)
table(train_data$Purchase, train_pred)
(28 + 111) / (463 + 28 + 111 + 198)

test_pred <- predict(svm_poly_best, test_data)
table(test_data$Purchase, test_pred)
(13 + 33) / (149 + 13 + 33 + 75)
