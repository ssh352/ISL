library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)

cor(Smarket)

cor(Smarket[,-9])

attach(Smarket)
plot(Volume)

glm.fits <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial)
summary(glm.fits)

coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[,4]

glm.probs <- predict(glm.fits, type = "response")
glm.probs[1:10]
contrasts(Direction)

glm.pred = rep("Down", 1250)
glm.pred[glm.probs>.5]="Up"
table(glm.pred, Direction)
mean(glm.pred == Direction)

train <- (Year < 2005)
Smarket.2005 <- Smarket[!train,]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]

glm.fits <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial, subset = train)
glm.probs <- predict(glm.fits, Smarket.2005, type = "response")

glm.pred = rep("Down", 252)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)

glm.fits <- glm(Direction ~ Lag1 + Lag2, data = Smarket, family = binomial, subset = train)
glm.probs <- predict(glm.fits, Smarket.2005, type = "response")
glm.pred = rep("Down", 252)
glm.pred[glm.probs > 0.5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
106 / (106 + 76)

predict(glm.fits, newdata = data.frame(Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -0.8)), type = "response")

library(MASS)
lda.fit = lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit
plot(lda.fit)

lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)

lda.class <- lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)
sum(lda.pred$posterior[,1] >= 0.5)
sum(lda.pred$posterior[,1] < 0.5)

lda.pred$posterior[1:20, 1]
lda.class[1:20]
sum(lda.pred$posterior[,1] >= 0.9)

qda.fit <- qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train) 
qda.fit

qda.class <- predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)

library(class)
train.X <- cbind(Lag1, Lag2)[train,]
test.X <- cbind(Lag1, Lag2)[!train,]
train.Direction <- Direction[train]

set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.2005)

knn.pred = knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)

dim(Caravan)
attach(Caravan)
summary(Purchase)

standardized.X <- scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])

test <- 1:1000
train.X <- standardized.X[-test,]
test.X <- standardized.X[test,]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k = 1)
mean(test.Y != knn.pred)
mean(test.Y != "No")
table(knn.pred, test.Y)

knn.pred <- knn(train.X, test.X, train.Y, k = 3)
table(knn.pred, test.Y)

knn.pred <- knn(train.X, test.X, train.Y, k = 5)
table(knn.pred, test.Y)

glm.fits <- glm(Purchase ~ ., data = Caravan, family = binomial, subset = -test)
glm.probs <- predict(glm.fits, Caravan[test,], type = "response")
glm.pred <- rep("No", 1000)
glm.pred[glm.probs > 0.5] <- "Yes"
table(glm.pred, test.Y)

glm.pred <- rep("No", 1000)
glm.pred[glm.probs > 0.25] <- "Yes"
table(glm.pred, test.Y)

library(ISLR)
attach(Weekly)
summary(Weekly)
pairs(Weekly)
cor(Weekly[,-9])

glm.fits <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = binomial)
summary(glm.fits)
glm.probs <- predict(glm.fits, type = "response")
glm.pred = rep("Down", 1089)
glm.pred[glm.probs>.5]="Up"
table(glm.pred, Direction)

train <- (Year < 2009)
Weekly.2009 <- Weekly[!train,]
dim(Weekly)
Direction.2009 <- Direction[!train]

glm.fits <- glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
summary(glm.fits)
glm.probs <- predict(glm.fits, Weekly.2009, type = "response")
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] <- "Up"
table(glm.pred, Direction.2009)
(31 + 17) / 104

library(MASS)
lda.fit <- lda(Direction ~ Lag2, subset = train)
lda.pred <- predict(lda.fit, Weekly.2009)
table(lda.pred$class, Direction.2009)
mean(lda.pred$class == Direction.2009)

qda.fit <- qda(Direction ~ Lag2, subset = train)
qda.pred <- predict(qda.fit, Weekly.2009)
table(qda.pred$class, Direction.2009)
mean(qda.pred$class == Direction.2009)

set.seed(1)
library(class)

train.X <- as.matrix(Lag2[train])
test.X <- as.matrix(Lag2[!train])
train.Y <- Direction[train]
test.Y <- Direction[!train]
knn.pred <- knn(train.X, test.X, train.Y, k = 1)
table(knn.pred, test.Y)
mean(knn.pred == Direction.2009)

knn.pred <- knn(train.X, test.X, train.Y, k = 10)
table(knn.pred, test.Y)

library(ISLR)
attach(Auto)
mpg01 <- rep(0, length(mpg))
mpg01[mpg > median(mpg)] <- 1
Auto <- data.frame(Auto, mpg01)

plot(mpg01, displacement)
plot(mpg01, cylinders)
plot(mpg01, horsepower)
plot(mpg01, weight)
plot(mpg01, acceleration)
plot(mpg01, year)
plot(mpg01, origin)
pairs(Auto)

train <- year %% 2 ==0
test <- !train
Auto.train <- Auto[train,]
Auto.test <- Auto[test,]

library(MASS) 
lda.fit <- lda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, subset = train)
lda.pred <- predict(lda.fit, Auto.test)
mpg01.test <- mpg01[test]
mean(lda.pred$class != mpg01.test)

qda.fit <- qda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, subset = train)
qda.pred <- predict(qda.fit, Auto.test)
mpg01.test <- mpg01[test]
mean(qda.pred$class != mpg01.test)

glm.fit <- glm(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, family = binomial, subset = train)
glm.probs = predict(glm.fit, Auto.test, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
mean(glm.pred != mpg01.test)

library(class)
train.X = cbind(cylinders, weight, displacement, horsepower)[train, ]
test.X = cbind(cylinders, weight, displacement, horsepower)[test, ]
train.mpg01 = mpg01[train]
set.seed(1)

knn.pred = knn(train.X, test.X, train.mpg01, k = 1)
mean(knn.pred != mpg01.test)

knn.pred = knn(train.X, test.X, train.mpg01, k = 2)
mean(knn.pred != mpg01.test)

knn.pred = knn(train.X, test.X, train.mpg01, k = 3)
mean(knn.pred != mpg01.test)

knn.pred = knn(train.X, test.X, train.mpg01, k = 4)
mean(knn.pred != mpg01.test)

knn.pred = knn(train.X, test.X, train.mpg01, k = 5)
mean(knn.pred != mpg01.test)

knn.pred = knn(train.X, test.X, train.mpg01, k = 6)
mean(knn.pred != mpg01.test)

Power <- function(){
  2^3
}
print(Power())
Power2 <- function(x, a){
  x^a
}
Power2(3,8)
Power2(10,3)
Power2(8,17)
Power2(131,3)

Power3 <- function(x, a){
  result <- x^a
  return(result)
}
print(Power3(3,8))

x <- 1:10
y <- Power3(x, 2)
plot(x,y)

plot(x,y, log = "x")
plot(x,y, log = "y")
plot(x,y, log = "xy")

PlotPower <- function(x,a){
  y <- Power3(x, a)
  plot(x,y)
}
PlotPower(1:10,3)

library(MASS)
summary(Boston)
attach(Boston)
crime01 = rep(0, length(crim))
crime01[crim > median(crim)] = 1
Boston = data.frame(Boston, crime01)

train = 1:(dim(Boston)[1]/2)
test = (dim(Boston)[1]/2 + 1):dim(Boston)[1]
Boston.train = Boston[train, ]
Boston.test = Boston[test, ]
crime01.test = crime01[test]
glm.fit = glm(crime01 ~ . - crime01 - crim, data = Boston, family = binomial, 
              subset = train)
glm.probs = predict(glm.fit, Boston.test, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
mean(glm.pred != crime01.test)

glm.fit = glm(crime01 ~ . - crime01 - crim - chas - tax, data = Boston, family = binomial, 
              subset = train)
glm.probs = predict(glm.fit, Boston.test, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
mean(glm.pred != crime01.test)

lda.fit = lda(crime01 ~ . - crime01 - crim, data = Boston, subset = train)
lda.pred = predict(lda.fit, Boston.test)
mean(lda.pred$class != crime01.test)

lda.fit = lda(crime01 ~ . - crime01 - crim - chas - tax, data = Boston, subset = train)
lda.pred = predict(lda.fit, Boston.test)
mean(lda.pred$class != crime01.test)

lda.fit = lda(crime01 ~ . - crime01 - crim - chas - tax - lstat - indus - age, 
              data = Boston, subset = train)
lda.pred = predict(lda.fit, Boston.test)
mean(lda.pred$class != crime01.test)

library(class)
train.X = cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, 
                lstat, medv)[train, ]
test.X = cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, 
               lstat, medv)[test, ]
train.crime01 = crime01[train]
set.seed(1)
# KNN(k=1)
knn.pred = knn(train.X, test.X, train.crime01, k = 1)
mean(knn.pred != crime01.test)

knn.pred = knn(train.X, test.X, train.crime01, k = 10)
mean(knn.pred != crime01.test)

knn.pred = knn(train.X, test.X, train.crime01, k = 100)
mean(knn.pred != crime01.test)

train.X = cbind(zn, nox, rm, dis, rad, ptratio, black, medv)[train, ]
test.X = cbind(zn, nox, rm, dis, rad, ptratio, black, medv)[test, ]
knn.pred = knn(train.X, test.X, train.crime01, k = 10)
mean(knn.pred != crime01.test)
