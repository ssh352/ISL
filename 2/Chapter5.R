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
