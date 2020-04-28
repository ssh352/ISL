library(ISLR)
set.seed(1)
train <- sample(392,196)

lm.fit = lm(mpg ~ horsepower, data = Auto, subset = train)

attach(Auto)
mean((mpg - predict(lm.fit, Auto))[-train]^2)

lm.fit2 <- lm(mpg ~ poly(horsepower,2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)

lm.fit3 <- lm(mpg ~ poly(horsepower,3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)

set.seed(2)
lm.fit = lm(mpg ~ horsepower, data = Auto, subset = train)
mean((mpg - predict(lm.fit, Auto))[-train]^2)

lm.fit2 <- lm(mpg ~ poly(horsepower,2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)

lm.fit3 <- lm(mpg ~ poly(horsepower,3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)

glm.fit <- glm(mpg ~ horsepower, data = Auto)
coef(glm.fit)

lm.fit <- lm(mpg ~ horsepower, data = Auto)
coef(lm.fit)

library(boot)
glm.fit <- glm(mpg ~ horsepower, data = Auto)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta

cv.error <- rep(0, 5)
for(i in 1:5){
  glm.fit <- glm(mpg ~ poly(horsepower,i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.error

set.seed(17)
cv.error.10 <- rep(0,10)
for(i in 1:10){
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}
cv.error.10

alpha.fn <- function(data, index){
  X <- data$X[index]
  Y <- data$Y[index]
  return ((var(Y) - cov(X,Y)) / (var(X) + var(Y) - 2 * cov(X,Y)))
}

alpha.fn(Portfolio, 1 :100)

set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace = T))

boot(Portfolio, alpha.fn, R = 1000)

boot.fn <- function(data, index){
  return(coef(lm(mpg ~ horsepower, data = data, subset = index)))
}
boot.fn(Auto, 1:392)

boot.fn(Auto, sample(392, 392, replace = T))
boot.fn(Auto, sample(392, 392, replace = T))

boot(Auto, boot.fn, 1000)

summary(lm(mpg ~ horsepower, data = Auto))$coef

boot.fn <- function(data, index){
  coefficients(lm(mpg ~ horsepower + I(horsepower ^ 2), data = data, subset = index))
}
boot(Auto, boot.fn, 1000)

store <- rep(NA, 10000)
for(i in 1:10000){
  store[i] <- sum(sample(1:100, rep = TRUE) == 4) > 0
}
mean(store)

library(ISLR)
attach(Default)
set.seed(1)

glm.fit <- glm(default ~ income + balance, family = binomial)
summary(glm.fit)

train <- sample(dim(Default)[1], dim(Default)[1]/2)
glm.fit <- glm(default ~ income + balance, family = binomial, data = Default, subset = train)
glm.pred <- rep("No", dim(Default)[1]/2)
glm.probs <- predict(glm.fit, Default[-train, ], type = "response")
glm.pred[glm.probs > 0.5] = "Yes"
mean(glm.pred != Default[-train, ]$default)

train <- sample(dim(Default)[1], dim(Default)[1]/2)
glm.fit <- glm(default ~ income + balance, family = binomial, data = Default, subset = train)
glm.pred <- rep("No", dim(Default)[1]/2)
glm.probs <- predict(glm.fit, Default[-train, ], type = "response")
glm.pred[glm.probs > 0.5] = "Yes"
mean(glm.pred != Default[-train, ]$default)

train <- sample(dim(Default)[1], dim(Default)[1]/2)
glm.fit <- glm(default ~ income + balance + student, family = binomial, data = Default, subset = train)
glm.pred <- rep("No", dim(Default)[1]/2)
glm.probs <- predict(glm.fit, Default[-train, ], type = "response")
glm.pred[glm.probs > 0.5] = "Yes"
mean(glm.pred != Default[-train, ]$default)

boot.func <- function(data, index){
  coef(glm(default ~ income + balance, family = binomial, subset = index))
}
library(boot)
boot(Default, boot.func, 50)

attach(Weekly)

glm.fit <- glm(Direction ~ Lag1 + Lag2, family = binomial)
glm.fit2 <- glm(Direction ~ Lag1 + Lag2, data = Weekly[-1,], family = binomial)
predict.glm(glm.fit2, Weekly[1, ], type = "response") > 0.5
Weekly[1, ]

n <- dim(Weekly)[1]
result <- rep(0, n)
for(i in 1:n){
  glm.fit2 <- glm(Direction ~ Lag1 + Lag2, data = Weekly[-i,], family = binomial)
  pred <- predict.glm(glm.fit2, Weekly[i, ], type = "response") > 0.5
  truth <- Weekly[i, ]$Direction == "Up"
  if(pred != truth){
    result[i] <- 1
  }
}

sum(result)

set.seed(1)
x <- rnorm(100)
y <- x - 2 * x^2 + rnorm(100)

plot(x , y)

data_frame <- data.frame(x,y)
glm.fit = glm(y ~ x)
library(boot)
cv.glm(data_frame, glm.fit)$delta

glm.fit = glm(y ~ poly(x, 2))
cv.glm(data_frame, glm.fit)$delta

glm.fit = glm(y ~ poly(x, 3))
cv.glm(data_frame, glm.fit)$delta

glm.fit = glm(y ~ poly(x, 4))
cv.glm(data_frame, glm.fit)$delta

set.seed(100)
x <- rnorm(100)
y <- x - 2 * x^2 + rnorm(100)

plot(x , y)

data_frame <- data.frame(x,y)
glm.fit <- glm(y ~ x)
library(boot)
cv.glm(data_frame, glm.fit)$delta

glm.fit <- glm(y ~ poly(x, 2))
cv.glm(data_frame, glm.fit)$delta

glm.fit <- glm(y ~ poly(x, 3))
cv.glm(data_frame, glm.fit)$delta

glm.fit <- glm(y ~ poly(x, 4))
cv.glm(data_frame, glm.fit)$delta

glm.fit <- glm(y ~ x)
summary(glm.fit)

library(MASS)
attach(Boston)
mean(medv)

sd(medv) / sqrt(length(medv))

boot.func <- function(data, index){
  return(mean(data[index]))
}
boot(medv, boot.func, 1000)
upper <- 22.53281 + 2 * 0.4189165
lower <- 22.53281 - 2 * 0.4189165

t.test(medv)

median(medv)

boot.func <- function(data, index){
  return(median(data[index]))
}
boot(medv, boot.func, 1000)

tenth <- quantile(medv, c(0.1))

boot.func <- function(data, index){
  return(quantile(data[index],c(0.1)))
}
boot(medv, boot.func, 1000)
