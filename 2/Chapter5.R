library(ISLR)
set.seed(1)
train <- sample(392, 196)

model_lm <- lm(mpg ~ horsepower, data = Auto, subset = train)
attach(Auto)
mean((mpg - predict(model_lm, Auto))[-train]^2)

model_lm2 <- lm(mpg ~ poly(horsepower,2), data = Auto, subset = train)
mean((mpg - predict(model_lm2, Auto))[-train]^2)

model_lm3 <- lm(mpg ~ poly(horsepower,3), data = Auto, subset = train)
mean((mpg - predict(model_lm3, Auto))[-train]^2)

set.seed(2)
train <- sample(392, 196)

model_lm <- lm(mpg ~ horsepower, data = Auto, subset = train)
attach(Auto)
mean((mpg - predict(model_lm, Auto))[-train]^2)

model_lm2 <- lm(mpg ~ poly(horsepower,2), data = Auto, subset = train)
mean((mpg - predict(model_lm2, Auto))[-train]^2)

model_lm3 <- lm(mpg ~ poly(horsepower,3), data = Auto, subset = train)
mean((mpg - predict(model_lm3, Auto))[-train]^2)

model_glm <- glm(mpg ~ horsepower, data = Auto)
coef(model_glm)

model_lm <- lm(mpg ~ horsepower, data = Auto)
coef(model_lm)

library(boot)
model_glm <- glm(mpg ~ horsepower, data = Auto)
cv_err <- cv.glm(Auto, model_glm)
cv_err$delta

cv_error <- rep(0, 5)
for(i in 1:5){
  model_glm <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv_error[i] <- cv.glm(Auto, model_glm)$delta[1]
}
cv_error

set.seed(10)
cv_error_10 <- rep(0, 10)
for(i in 1:10){
  model_fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv_error_10[i] <- cv.glm(Auto, model_fit, K = 10)$delta[1]
}
cv_error_10

alpha_fn <- function(data, index){
  X <- data$X[index]
  Y <- data$Y[index]
  return((var(Y) - cov(X, Y))/(var(X) + var(Y) - 2*cov(X, Y)))
}
alpha_fn(Portfolio, 1:100)

set.seed(1)
alpha_fn(Portfolio, sample(100,100, replace = T))
boot(Portfolio, alpha_fn, R = 1000)

boot_fn <- function(data, index){
  return(coef(lm(mpg ~ horsepower, data = data, subset = index)))
}
boot_fn(Auto, 1: 392)
boot_fn(Auto, sample(392, 392, replace = T))
boot(Auto, boot_fn, 1000)

summary(lm(mpg ~ horsepower, data = Auto))$coef

boot_fn <- function(data, index){
  return(coef(lm(mpg ~ horsepower + I(horsepower ^ 2), data = data, subset = index)))
}
boot(Auto, boot_fn, 1000)
summary(lm(mpg ~ horsepower + I(horsepower ^ 2), data = Auto))$coef

x <- 1:10000
P <- 1 - ( 1 - 1/x)^x
plot(P)

store <- rep(NA, 10000)
for(i in 1:10000){
  store[i] <- sum(sample(1:100, rep = T) == 4)>0
}
mean(store)

library(ISLR)
attach(Default)

model_logi <- glm(default ~ income + balance, data = Default, family = binomial)

HoloOut <- function(){
  train <- sample(dim(Default)[1], dim(Default)[1]/2)
  model <- glm(default ~ income + balance, data = Default, family = binomial, subset = train)
  pred <- rep("No", dim(Default)[1]/2)
  prob <- predict(model, Default[-train,], type = "response")
  pred[prob > 0.5] <- "Yes"
  return(mean(pred != Default[-train,]$default))
}
HoloOut()

train <- sample(dim(Default)[1], dim(Default)[1]/2)
model <- glm(default ~ income + balance + student, data = Default, family = binomial, subset = train)
pred <- rep("No", dim(Default)[1]/2)
prob <- predict(model, Default[-train,], type = "response")
pred[prob > 0.5] <- "Yes"
mean(pred != Default[-train,]$default)

model_logi <- glm(default ~ income + balance, data = Default, family = binomial)
summary(model_logi)

boot_fn <- function(data, index){
  return(coef(glm(default ~ income + balance, data = data, family = binomial, subset = index)))
}

boot(Default, boot_fn, R = 50)

library(ISLR)
attach(Weekly)
model_logi <- glm(Direction ~ Lag1 + Lag2, data = Weekly, family = binomial)
summary(model_logi)

model_logi1 <- glm(Direction ~ Lag1 + Lag2, data = Weekly[-1,], family = binomial)
summary(model_logi1)
prob <- predict(model_logi1, Weekly[1,], type = "response")
prob
Weekly[1,]$Direction

result <- rep(NA, dim(Weekly)[1])
for(i in 1:dim(Weekly)[1]){
  model <- glm(Direction ~ Lag1 + Lag2, data = Weekly[-i,], family = binomial)
  summary(model)
  prob <- predict(model, Weekly[i,], type = "response")
  pred <- ifelse(prob > 0.5, "Up", "Down")
  result[i] <- ifelse(pred == Weekly[i,]$Direction, 1, 0)
}
mean(result)

set.seed(1)
x <- rnorm(100)
y <- x - 2*x^2 + rnorm(100)
plot(x, y)

library(boot)
Data = data.frame(x, y)
model <- glm(y ~ x)
cv.glm(Data, model)$delta

model <- glm(y ~ poly(x, 2))
cv.glm(Data, model)$delta

model <- glm(y ~ poly(x, 3))
cv.glm(Data, model)$delta

model <- glm(y ~ poly(x, 4))
cv.glm(Data, model)$delta

set.seed(100)
model <- glm(y ~ x)
cv.glm(Data, model)$delta

model <- glm(y ~ poly(x, 2))
cv.glm(Data, model)$delta

model <- glm(y ~ poly(x, 3))
cv.glm(Data, model)$delta

model <- glm(y ~ poly(x, 4))
cv.glm(Data, model)$delta

model <- glm(y ~ x)
summary(model)
model <- glm(y ~ poly(x, 2))
summary(model)
model <- glm(y ~ poly(x, 3))
summary(model)
model <- glm(y ~ poly(x, 4))
summary(model)

library(MASS)
attach(Boston)
mean <- mean(medv)
medv_err <- sd(medv)/sqrt(length(medv))

boot_mean <- function(data, index){
  return(mean(data[index]))
}
library(boot)
boot(medv, boot_mean, R = 1000)
t.test(medv)
c(22.53 - 2 * 0.4187 , 22.53 + 2 * 0.4187)

medv_med <- median(medv)

boot_median <- function(data, index){
  return(median(data[index]))
}
boot(medv, boot_median, R = 1000)

medv_ten <- quantile(medv, c(0.1))

boot_ten <- function(data, index){
  return(quantile(data[index], c(0.1)))
}
boot(medv, boot_ten, R = 1000)
