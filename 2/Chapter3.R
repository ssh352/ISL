library(MASS)
library(ISLR)

names(Boston)

model_lm <- lm(medv ~ lstat, data = Boston)
attach(Boston)

model_lm
summary(model_lm)
names(model_lm)
confint(model_lm)

predict(model_lm, data.frame(lstat = c(5, 10, 15)), interval = "confidence")
predict(model_lm, data.frame(lstat = c(5, 10, 15)), interval = "prediction")
plot(lstat, medv)
abline(model_lm)

abline(model_lm, lwd = 3)
abline(model_lm, lwd = 3, col = "red")
plot(lstat, medv, col = "red")
plot(lstat, medv, pch = 20)
plot(lstat, medv, pch = "+")
plot(1:20, 1:20, pch = 1:20)

par(mfrow = c(2,2))
plot(model_lm)

par(mfrow = c(1,1))
plot(predict(model_lm), residuals(model_lm))
plot(predict(model_lm), rstudent(model_lm))
plot(hatvalues(model_lm))
which.max(hatvalues(model_lm))

model_lm <- lm(medv ~ lstat + age, data = Boston)
summary(model_lm)

model_lm <- lm(medv ~ ., data = Boston)
summary(model_lm)

library(car)
vif(model_lm)

model_lm1 <- lm(medv ~ .-age, data = Boston)
summary(model_lm1)

summary(lm(medv ~ lstat * age, data = Boston))

model_lm2 <- lm(medv ~ lstat + I(lstat ^ 2))
summary(model_lm2)
model_lm <- lm(medv ~ lstat)
anova(model_lm, model_lm2)

par(mfrow = c(2,2))
plot(model_lm2)

model_lm5 <- lm(medv ~ poly(lstat,5))
summary(model_lm5)
summary(lm(medv ~ log(rm)))

model_lm <- lm(Sales ~ . + Income:Advertising + Price: Age, data = Carseats)
summary(model_lm)
attach(Carseats)
contrasts(ShelveLoc)

library(ISLR)
attach(Auto)
Auto = na.omit(Auto)

model_lm <- lm(mpg ~ horsepower, data = Auto)
summary(model_lm)

predict(model_lm, data.frame(horsepower = c(98)), interval = "confidence")
predict(model_lm, data.frame(horsepower = c(98)), interval = "prediction")       

plot(horsepower, mpg)
abline(model_lm)

par(mfrow = c(2,2))
plot(model_lm)

pairs(Auto)

cor(subset(Auto, select=-name))

model_lm <- lm(mpg ~ .-name, data = Auto)
summary(model_lm)
plot(model_lm)

attach(Carseats)
model_lm <- lm(Sales ~ Price + Urban + US, data = Carseats)
summary(model_lm)

model_lm2 <- lm(Sales ~ Price + US, data = Carseats)
summary(model_lm2)

confint(model_lm2)

plot(model_lm2)

set.seed(1)
x <- rnorm(100)
y <- 2 * x + rnorm(100)
model_lm <- lm(y ~ x + 0)
summary(model_lm)

model_lm2 <- lm(x ~ y + 0)
summary(model_lm2)

(sqrt(length(x)-1) * sum(x*y)) / (sqrt(sum(x*x) * sum(y*y) - (sum(x*y))^2))

model_lm <- lm(y ~ x)
summary(model_lm)

model_lm2 <- lm(y ~ x)
summary(model_lm2)

set.seed(1)
x <- rnorm(100)
y <- 2 * x
model_lm <- lm(y ~ x + 0)
model_lm2 <- lm(x ~ y + 0)
summary(model_lm)
summary(model_lm2)

set.seed(1)
x <- rnorm(100)
y <- -sample(x, 100)
sum(x ^ 2)
sum(y ^ 2)
model_lm <- lm(y ~ x + 0)
model_lm2 <- lm(x ~ y + 0)
summary(model_lm)
summary(model_lm2)

set.seed(1)
x <- rnorm(100)
eps <- rnorm(100, sd = 0.05)
y <- -1 + 0.5 * x + eps
plot(x, y)

model_lm <- lm(y ~ x)
summary(model_lm)

plot(x, y)
abline(model_lm, col = "red")
abline(-1, 0.5)

model_sq <- lm(y ~ poly(x,2))
summary(model_sq)

set.seed(1)
x1 <- runif(100)
x2 <- 0.5 * x1 + rnorm(100)/ 10
y <- 2 + 2*x1 + 0.3*x2 + rnorm(100)
cor(x1, x2)
plot(x1, x2)

model_lm <- lm(y ~ x1 + x2)
summary(model_lm)

model_lm1 <- lm(y ~ x1)
summary(model_lm1)

model_lm2 <- lm(y ~ x2)
summary(model_lm2)

x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y <- c(y, 6)

model_lm <- lm(y ~ x1 + x2)
summary(model_lm)
plot(model_lm)

model_lm1 <- lm(y ~ x1)
summary(model_lm1)
plot(model_lm1)

model_lm2 <- lm(y ~ x2)
summary(model_lm2)
plot(model_lm2)

library(MASS)
attach(Boston)

Boston$chas <- factor(Boston$chas, labels = c("N","Y"))
summary(Boston)

model_zn <- lm(crim ~ zn)
summary(model_zn)

model_indus <- lm(crim ~ indus)
summary(model_indus)

model_chas <- lm(crim ~ chas)
summary(model_chas)

model_nox <- lm(crim ~ nox)
summary(model_nox)

model_rm <- lm(crim ~ rm)
summary(model_rm)

model_age <- lm(crim ~ age)
summary(model_age)

model_dis <- lm(crim ~ dis)
summary(model_dis)

model_rad <- lm(crim ~ rad)
summary(model_rad)

model_tax <- lm(crim ~ tax)
summary(model_tax)

model_ptratio <- lm(crim ~ ptratio)
summary(model_ptratio)

model_black <- lm(crim ~ black)
summary(model_black)

model_lstat <- lm(crim ~ lstat)
summary(model_lstat)

model_medv <- lm(crim ~ medv)
summary(model_medv)

model_all <- lm(crim ~ ., data = Boston)

summary(model_all)

x <- c(coefficients(model_zn)[2],
      coefficients(model_indus)[2],
      coefficients(model_chas)[2],
      coefficients(model_nox)[2],
      coefficients(model_rm)[2],
      coefficients(model_age)[2],
      coefficients(model_dis)[2],
      coefficients(model_rad)[2],
      coefficients(model_tax)[2],
      coefficients(model_ptratio)[2],
      coefficients(model_black)[2],
      coefficients(model_lstat)[2],
      coefficients(model_medv)[2])
y <- coefficients(model_all)[2:14]
plot(x, y)

model_zn <- lm(crim ~ poly(zn,3))
summary(model_zn)

model_indus <- lm(crim ~ poly(indus,3))
summary(model_indus)

model_chas <- lm(crim ~ poly(chas,3))
summary(model_chas)

model_nox <- lm(crim ~ poly(nox,3))
summary(model_nox)

model_rm <- lm(crim ~ poly(rm,3))
summary(model_rm)

model_age <- lm(crim ~ poly(age,3))
summary(model_age)

model_dis <- lm(crim ~ poly(dis,3))
summary(model_dis)

model_rad <- lm(crim ~ poly(rad,3))
summary(model_rad)

model_tax <- lm(crim ~ poly(tax,3))
summary(model_tax)

model_ptratio <- lm(crim ~ poly(ptratio,3))
summary(model_ptratio)

model_black <- lm(crim ~ poly(black,3))
summary(model_black)

model_lstat <- lm(crim ~ poly(lstat,3))
summary(model_lstat)

model_medv <- lm(crim ~ poly(medv,3))
summary(model_medv)

