library(MASS)
library(ISLR)

data("Boston")
names(Boston)

attach(Boston)
lm.fit = lm(medv ~ lstat)
lm.fit
summary(lm.fit)

names(lm.fit)
coef(lm.fit)

confint(lm.fit)

predict(lm.fit, data.frame(lstat = c(5, 10, 15)),interval = "confidence")
predict(lm.fit, data.frame(lstat = c(5, 10, 15)),interval = "prediction")

plot(lstat, medv)
abline(lm.fit)

abline(lm.fit, lwd = 3)
abline(lm.fit, lwd = 3, col = "red")
plot(lstat, medv, col = "red")
plot(lstat, medv, pch = 20)
plot(lstat, medv, pch = "+")
plot(1:20, 1:20, pch = 1:20)

par(mfrow = c(2,2))
plot(lm.fit)

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

lm.fit = lm(medv ~ lstat + age)
summary(lm.fit)

lm.fit = lm(medv ~ ., data = Boston)
summary(lm.fit)
summary(lm.fit)$r.sq
summary(lm.fit)$sigma

library(car)
vif(lm.fit)

lm.fit = lm(medv ~ .-age, data = Boston)
summary(lm.fit)

summary(lm(medv ~ lstat * age))
lm.fit2 = lm(medv ~ lstat + I(lstat^2))
summary(lm.fit2)

lm.fit = lm(medv ~ lstat)
anova(lm.fit, lm.fit2)

par(mfrow = c(2,2))
plot(lm.fit2)

lm.fit5 = lm(medv ~ poly(lstat, 5))
summary(lm.fit5)

summary(lm(medv ~ log(rm)))
plot(lm(medv ~ log(rm)))

data("Carseats")
attach(Carseats)
names(Carseats)

lm.fit = lm(Sales ~ . + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)
contrasts(ShelveLoc)

LoadLibraries = function(){
  library(MASS)
  library(ISLR)
  print("The libraries have been loaded.")
}
LoadLibraries()

data(Auto)
attach(Auto)
Auto = na.omit(Auto)
lm.fit = lm(mpg ~ horsepower)
summary(lm.fit)

predict(lm.fit, data.frame(horsepower = c(98)),interval = "prediction")
plot(horsepower, mpg)
aline(lm.fit)
abline(lm.fit)


pairs(Auto)
cor(subset(Auto, select=-name))
lm.fit = lm(mpg ~ .-name, data = Auto)
summary(lm.fit)

par(mfrow=c(2,2))
plot(lm.fit)
par(mfrow = c(1))
plot(predict(lm.fit), rstudent(lm.fit))


data("Carseats")
attach(Carseats)

lm.fit = lm(Sales ~ Price + Urban + US)
summary(lm.fit)

lm.fit2 = lm(Sales ~ Price + US)
summary(lm.fit2)

plot(predict(lm.fit2), rstudent(lm.fit2))
par(mfrow=c(2,2))
plot(lm.fit2)

set.seed(1)
x = rnorm(100)
y = 2 * x + rnorm(100)

lm.fit = lm(y ~ x + 0)
summary(lm.fit)

lm.fit2 = lm(x ~ y + 0)
summary(lm.fit2)

(sqrt(length(x)-1) * sum(x*y)) / (sqrt(sum(x*x) * sum(y*y) - (sum(x*y))^2))

lm.fit = lm(y ~ x)
lm.fit2 = lm(x ~ y)
summary(lm.fit)
summary(lm.fit2)

x = rnorm(100)
y = 2 * x
lm.fit = lm(y ~ x + 0)
lm.fit2 = lm(x ~ y + 0)
summary(lm.fit)
summary(lm.fit2)

x = rnorm(100)
y = rnorm(100)
lm.fit = lm(y ~ x + 0)
lm.fit2 = lm(x ~ y + 0)
summary(lm.fit)
summary(lm.fit2)


set.seed(1)
x <- rnorm(100)
eps <- rnorm(100, mean = 0, sd = 0.25)
y <- -1 + 0.5 * x + eps
par(mfrow=c(1,1))
plot(x, y)
lm.fit <- lm(y ~ x)
summary(lm.fit)

plot(x, y)
abline(-1.00942, 0.49973, col = "red")

lm.fit = lm(y ~ x + I(x^2))
summary(lm.fit)

eps <- rnorm(100, mean = 0, sd = 0.25)
y <- -1 + 0.5 * x + eps
par(mfrow=c(1,1))
plot(x, y)
lm.fit <- lm(y ~ x)
summary(lm.fit)

eps <- rnorm(100, mean = 0, sd = 0.05)
y <- -1 + 0.5 * x + eps
par(mfrow=c(1,1))
plot(x, y)
lm.fit2 <- lm(y ~ x)
summary(lm.fit2)

eps <- rnorm(100, mean = 0, sd = 0.55)
y <- -1 + 0.5 * x + eps
par(mfrow=c(1,1))
plot(x, y)
lm.fit5 <- lm(y ~ x)
summary(lm.fit5)

confint(lm.fit)
confint(lm.fit2)
confint(lm.fit5)

set.seed(1)
x1 = runif(100)
x2 = 0.5 * x1 + rnorm(100) / 10
y = 2 + 2 * x1 + 0.3 * x2 + rnorm(100)

cor(x1, x2)
plot(x1, x2)

lm.fit = lm(y ~ x1 + x2)
summary(lm.fit)

lm.fit2 = lm(y ~ x1)
summary(lm.fit2)

lm.fit5 = lm(y ~ x2)
summary(lm.fit5)

x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y, 6)

cor(x1, x2)
plot(x1, x2)

lm.fit = lm(y ~ x1 + x2)
summary(lm.fit)
par(mfrow = c(2,2))
plot(lm.fit)

lm.fit2 = lm(y ~ x1)
summary(lm.fit2)

lm.fit5 = lm(y ~ x2)
summary(lm.fit5)

library(MASS)
data("Boston")
attach(Boston)
summary(Boston)

lm.zn = lm(crim ~ zn)
summary(lm.zn)
plot(lm.zn)

lm.indus = lm(crim ~ indus)
summary(lm.indus)
plot(lm.indus)

lm.chas = lm(crim ~ chas)
summary(lm.chas)
plot(lm.chas)

lm.nox = lm(crim ~ nox)
summary(lm.nox)
plot(lm.nox)

lm.rm = lm(crim ~ rm)
summary(lm.rm)
plot(lm.rm)

lm.age = lm(crim ~ age)
summary(lm.age)
plot(lm.age)

lm.dis = lm(crim ~ dis)
summary(lm.dis)
plot(lm.dis)

lm.rad = lm(crim ~ rad)
summary(lm.rad)
plot(lm.rad)

lm.tax = lm(crim ~ tax)
summary(lm.tax)
plot(lm.tax)

lm.ptratio = lm(crim ~ ptratio)
summary(lm.ptratio)
plot(lm.ptratio)

lm.black = lm(crim ~ black)
summary(lm.black)
plot(lm.black)

lm.lstat = lm(crim ~ lstat)
summary(lm.lstat)
plot(lm.lstat)

lm.medv = lm(crim ~ medv)
summary(lm.medv)
plot(lm.medv)

lm.all = lm(crim ~ ., data = Boston)
summary(lm.all)
plot(lm.all)

x = c(coefficients(lm.zn)[2],
      coefficients(lm.indus)[2],
      coefficients(lm.chas)[2],
      coefficients(lm.nox)[2],
      coefficients(lm.rm)[2],
      coefficients(lm.age)[2],
      coefficients(lm.dis)[2],
      coefficients(lm.rad)[2],
      coefficients(lm.tax)[2],
      coefficients(lm.ptratio)[2],
      coefficients(lm.black)[2],
      coefficients(lm.lstat)[2],
      coefficients(lm.medv)[2])
y = coefficients(lm.all)[2:14]
plot(x, y)

lm.zn = lm(crim~poly(zn,3))
summary(lm.zn) 

lm.indus = lm(crim~poly(indus,3))
summary(lm.indus) 

lm.nox = lm(crim~poly(nox,3))
summary(lm.nox) 

lm.rm = lm(crim~poly(rm,3))
summary(lm.rm) 

lm.age = lm(crim~poly(age,3))
summary(lm.age) 

lm.dis = lm(crim~poly(dis,3))
summary(lm.dis) 

lm.rad = lm(crim~poly(rad,3))
summary(lm.rad) 

lm.tax = lm(crim~poly(tax,3))
summary(lm.tax) 

lm.ptratio = lm(crim~poly(ptratio,3))
summary(lm.ptratio) 

lm.black = lm(crim~poly(black,3))
summary(lm.black) 

lm.lstat = lm(crim~poly(lstat,3))
summary(lm.lstat) 

lm.medv = lm(crim~poly(medv,3))
summary(lm.medv) 
