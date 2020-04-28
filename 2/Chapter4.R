library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)

cor(Smarket)
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)

model_logi <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
                  data = Smarket, family = binomial)
summary(model_logi)
coef(model_logi)

summary(model_logi)$coef
summary(model_logi)$coef[,4]

probs <- predict(model_logi,type = "response")
probs[1:10]

contrasts(Direction)

pred <- rep("Down", 1250)
pred[probs > 0.5] <- "Up"

table(pred, Direction)
(145+507)/1250

train <- (Year < 2005)
Smarket_2005 <- Smarket[!train,]
dim(Smarket_2005)
Direction_2005 <- Direction[!train]

model_logi <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
                  data = Smarket, family = binomial, subset = train)
probs <- predict(model_logi, Smarket_2005 ,type = "response")
pred <- rep("Down", 252)
pred[probs > 0.5] <- "Up"
table(pred, Direction_2005)

mean(pred == Direction_2005)
mean(pred != Direction_2005)

model_logi <- glm(Direction ~ Lag1 + Lag2 , 
                  data = Smarket, family = binomial, subset = train)
probs <- predict(model_logi, Smarket_2005 ,type = "response")
pred <- rep("Down", 252)
pred[probs > 0.5] <- "Up"
table(pred, Direction_2005)

mean(pred == Direction_2005)
mean(pred != Direction_2005)

predict(model_logi, newdata = data.frame(Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -0.8)), type = "response")

library(MASS)

model_lda <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
model_lda

plot(model_lda)

pred <- predict(model_lda, Smarket_2005)
names(pred)

pred_class <- pred$class
table(pred_class, Direction_2005)
mean(pred_class == Direction_2005)

sum(pred$posterior[,1] >= 0.5)
sum(pred$posterior[,1] < 0.5)

pred$posterior[1:20]
pred_class[1:20]

sum(pred$posterior[,1] >= 0.9)

model_qda <- qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
model_qda

qda_class <- predict(model_qda, Smarket_2005)$class
table(qda_class, Direction_2005)
mean(qda_class == Direction_2005)

library(class)
train_X <- cbind(Lag1, Lag2)[train,]
test_X <- cbind(Lag1, Lag2)[!train,]
train_Direction <- Direction[train]

set.seed(1)
model_knn <- knn(train_X, test_X, train_Direction, k = 1)
table(model_knn, Direction_2005)
(83 + 43)/252

model_knn <- knn(train_X, test_X, train_Direction, k = 3)
table(model_knn, Direction_2005)
(48 + 86)/252

dim(Caravan)
attach(Caravan)
summary(Purchase)
348/(5474 + 348)

standarized_X <- scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standarized_X[,1])
var(standarized_X[,2])

test <- 1:1000
train_X <- standarized_X[-test,]
test_X <- standarized_X[test,]
train_Y <- Purchase[-test]
test_Y <- Purchase[test]

pred <- knn(train_X, test_X, train_Y, k = 1)
mean(test_Y != pred)
mean(test_Y == pred)

table(pred, test_Y)
9 / (66 + 9)

pred <- knn(train_X, test_X, train_Y, k = 3)
table(pred, test_Y)
5/(21 + 5)

pred <- knn(train_X, test_X, train_Y, k = 5)
table(pred, test_Y)
4 / (11 + 4)

model_logi <- glm(Purchase ~ ., data = Caravan, family = binomial, subset = -test)
probs <- predict(model_logi, Caravan[test,], type = "response")
pred <- rep("No", 1000)
pred[probs > 0.5] <- "Yes"
table(pred, test_Y)
pred[probs > 0.25] <- "Yes"
table(pred, test_Y)

library(ISLR)
attach(Weekly)
pairs(Weekly)
names(Weekly)
cor(Weekly[,-9])

model_logi <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
                  data = Weekly, family = binomial)
summary(model_logi)

probs <- predict(model_logi, type = "response")
pred <- rep("DOWN", length(probs))
pred[probs > 0.5] <- "Up"
table(pred, Direction)
(54 + 557) / length(probs)

train <- Year <= 2008
train_data <- Weekly[train,]
test_data <- Weekly[!train,]
model_logi <- glm(Direction ~ Lag2,
                  data = Weekly, family = binomial, subset = train)
probs <- predict(model_logi, test_data ,type = "response")
pred <- rep("DOWN", length(probs))
pred[probs > 0.5] <- "Up"
table(pred, Direction[!train])
mean(pred ==  Direction[!train])

library(MASS)
model_lda <- lda(Direction ~ Lag2,data = Weekly, subset = train)
pred <- predict(model_lda, test_data)
table(pred$class, Direction[!train])
mean(pred$class == Direction[!train])

model_qda <- qda(Direction ~ Lag2,data = Weekly, subset = train)
pred <- predict(model_qda, test_data)
table(pred$class, Direction[!train])
mean(pred$class == Direction[!train])

library(class)
train_X <- as.matrix(Lag2[train])
test_X <- as.matrix(Lag2[!train])
train_Y <- Direction[train]
test_Y <- Direction[!train]
set.seed(1)
model_kn <- knn(train_X, test_X, train_Y, k = 1)
table(model_kn, test_Y)
mean(model_kn == test_Y)

library(ISLR)
attach(Auto)

mpg_median <- median(mpg)
mpg01 <- ifelse(mpg > mpg_median, 1, 0)
mpg01

Auto <- data.frame(Auto, mpg01)
pairs(Auto)
cor(Auto[,-9])

train <- year %% 2 == 0
test <- !train 
train_data <- Auto[train,]
test_data <- Auto[test,]
mpg01_test <- mpg01[test]

library(MASS)
model_lda <- lda(mpg01 ~ cylinders + displacement + horsepower + weight,
                 data = train_data)
pred <- predict(model_lda, test_data)
mean(pred$class != mpg01_test)

model_qda <- qda(mpg01 ~ cylinders + displacement + horsepower + weight,
                 data = train_data)
pred <- predict(model_qda, test_data)
mean(pred$class != mpg01_test)

model_logi <- glm(mpg01 ~ cylinders + displacement + horsepower + weight,
                  data = train_data, family = binomial)
prob <- predict(model_logi, test_data, type = "response")
pred <- rep(0, length(prob))
pred[prob > 0.5] <- 1
mean(pred != mpg01_test)

library(class)
train_data <- cbind(cylinders, displacement, horsepower, weight)[train,]
test_data <- cbind(cylinders, displacement, horsepower, weight)[test,]
mpg01_train <- mpg01[train]
set.seed(1)
model_knn <- knn(train_data, test_data, mpg01_train, k = 1)
mean(model_knn != mpg01_test)

model_knn <- knn(train_data, test_data, mpg01_train, k = 10)
mean(model_knn != mpg01_test)

Power = function(){
   2 ^ 3
}
Power()

Power2 <- function(x, a){
  x ^ a
}
Power2(3,8)
Power2(10,3)
Power2(8,17)
Power2(131,3)

Power3 <- function(x, a){
  result <- x ^ a
  return(result)
}
x <- 1:10
plot(x, Power3(x,2))
plot(x, Power3(x,2), log = "xy")

PlotPower <- function(x, a){
  plot(x, Power3(x,a))
}
PlotPower(1:10, 3)
