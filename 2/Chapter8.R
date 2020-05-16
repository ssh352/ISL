library(tree)
library(ISLR)
attach(Carseats)
High <- ifelse(Sales <= 8, "No", "Yes")

Carseats <- data.frame(Carseats, High)

tree_carseats <- tree(High ~ .-Sales, Carseats)
summary(tree_carseats)
plot(tree_carseats)
text(tree_carseats, pretty = 0)
tree_carseats

set.seed(2)
train <- sample(1:nrow(Carseats), 200)
test_data <- Carseats[-train,]
test_High <- High[-train]
tree_carseats <- tree(High ~ .-Sales, Carseats, subset = train)
pred <- predict(tree_carseats, test_data, type = "class")
table(pred, test_High)
(104 + 50) / 200

set.seed(3)
cv_carseats <- cv.tree(tree_carseats, FUN = prune.misclass)
names(cv_carseats)
cv_carseats
par(mfrow = c(1, 2))
plot(cv_carseats$size, cv_carseats$dev, type = "b")
plot(cv_carseats$k, cv_carseats$dev, type = "b")

prune.carseats <- prune.misclass(tree_carseats, best = 9)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
pred <- predict(prune.carseats, test_data, type = "class")
table(pred,test_High)
(97 + 58)/200


prune.carseats <- prune.misclass(tree_carseats, best = 15)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
pred <- predict(prune.carseats, test_data, type = "class")
table(pred,test_High)
(102 + 53)/200

library(MASS)
library(tree)
attach(Boston)
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
tree_boston <- tree(medv ~ ., Boston, subset = train)
summary(tree_boston)
plot(tree_boston)
text(tree_boston)

cv_boston <- cv.tree(tree.boston)
plot(cv_boston$size, cv_boston$dev, type = "b")

prune_boston <- prune.tree(tree_boston, best = 5)
plot(prune_boston)
text(prune_boston, pretty = 0)
yhat <- predict(tree_boston, newdata = Boston[-train,])
test_medv <- Boston[-train, "medv"]
plot(yhat, test_medv)
abline(0,1)
mean((yhat - test_medv)^2)

library(randomForest)
set.seed(1)
model_bag <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 13, importance = T)
model_bag

yhat_bag <- predict(model_bag, newdata = Boston[-train,])
plot(yhat_bag, test_medv)
abline(0,1)
mean((yhat_bag - test_medv)^2)

model_bag <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 13, ntree = 25)
yhat_bag <- predict(model_bag, newdata = Boston[-train,])
mean((yhat_bag - test_medv)^2)

set.seed(1)
model_rf <- randomForest(medv ~., data = Boston, subset = train, mtry = 6, importance = T)
yhat_rf <- predict(model_rf, newdata = Boston[-train,])
mean((yhat_rf - test_medv)^2)
importance(model_rf)
varImpPlot(model_rf)

library(gbm)
set.seed(1)
model_boost <- gbm(medv ~ ., data = Boston[train,], distribution = "gaussian", n.trees = 5000,
                   interaction.depth = 4)
summary(model_boost)
par(mfrow = c(1, 2))
plot(model_boost, i = "rm")
plot(model_boost, i = "lstat")

yhat_boost <- predict(model_boost, newdata = Boston[-train,], n.trees = 5000)
mean((yhat_boost - test_medv)^2)

model_boost <- gbm(medv ~ ., data = Boston[train, ], distribution = "gaussian",
                   n.trees = 5000, interaction.depth = 4, shrinkage = 0.2, verbose = F)
yhat_boost <- predict(model_boost, newdata = Boston[-train,], n.trees = 5000)
mean((yhat_boost - test_medv)^2)

p <- seq(0, 1, 0.01)
gini <- p * (1-p) * 2
error <- 1 - pmax(p, 1-p)
entropy <- -(p * log(p) + (1-p) * log(1-p))
matplot(p, cbind(gini, error, entropy), col = c("red", "green", "blue"))

library(randomForest)
library(ISLR)
attach(Boston)

train <- sample(1 : nrow(Boston), nrow(Boston) / 2)
train_data <- Boston[train, -14]
test_data <- Boston[-train, -14]
train_Y <- Boston[train, 14]
test_Y <- Boston[-train, 14]

p <- dim(Boston)[2]-1
p_2 <- p/2
p_sq <- sqrt(p)

rf_p <- randomForest(train_data, train_Y, xtest = test_data, ytest = test_Y, 
                     mtry = p, ntree = 500)
rf_p_2 <- randomForest(train_data, train_Y, xtest = test_data, ytest = test_Y, 
                     mtry = p_2, ntree = 500)
rf_p_sq <- randomForest(train_data, train_Y, xtest = test_data, ytest = test_Y, 
                     mtry = p_sq, ntree = 500)

plot(1:500, rf_p$test$mse, col = "black",ylim = c(15, 40), type = "l")
lines(1:500, rf_p_2$test$mse, col = "red", type = "l")
lines(1:500, rf_p_sq$test$mse, col = "blue", type = "l")

library(ISLR)
attach(Carseats)
set.seed(1)

train <- sample(dim(Carseats)[1], dim(Carseats)[1]/2)
test <- -train
train_data <- Carseats[train,]
test_data <- Carseats[test,]

library(tree)
model_tree <- tree(Sales ~ ., data = train_data)
summary(model_tree)

plot(model_tree)
text(model_tree, pretty = 0)

pred <- predict(model_tree, test_data)
mean((pred - test_data$Sales)^2)

cv_tree <- cv.tree(model_tree, FUN=prune.tree)
par(mfrow = c(1,2))
plot(cv_tree$size, cv_tree$dev, type = "b")
plot(cv_tree$k, cv_tree$dev, type = "b")

pruned_tree <- prune.tree(model_tree, best = 9)

pruned_pred <- predict(pruned_tree, test_data)
mean((pruned_pred - test_data$Sales)^2)

library(randomForest)

model_bag <- randomForest(Sales ~ ., data = train_data, mtry = 10, mtree = 500, importance = T )
pred_bag <- predict(model_bag, test_data)
mean((pred_bag - test_data$Sales)^2)

model_rf <- randomForest(Sales ~ ., data = train_data, mtry = 5, mtree = 500, importance = T )
pred_rf <- predict(model_rf, test_data)
mean((pred_rf - test_data$Sales)^2)

attach(OJ)
set.seed(1)

train <- sample(dim(OJ)[1], 800)
test<- -train
train_data <- OJ[train,]
test_data<- OJ[test,]

model_tree <- tree(Purchase ~ ., data = train_data)
summary(model_tree)

model_tree

plot(model_tree)
text(model_tree)

pred <- predict(model_tree, test_data, type = "class")
table(pred, test_data$Purchase)
(8 + 38)/270

cv_tree <- cv.tree(model_tree, FUN = prune.tree)
plot(cv_tree$size, cv_tree$dev)

pruned_tree <- prune.tree(model_tree, best = 6)
summary(pruned_tree)

pred_pruned <- predict(pruned_tree, test_data, type = "class")
table(pred_pruned, test_data$Purchase)
(34 + 16)/270

library(ISLR)
attach(Hitters)
Hitters <- Hitters[-which(is.na(Hitters$Salary)), ]
sum(is.na(Hitters$Salaly))
Hitters$Salary <- log(Hitters$Salary)

train <- 1:200
train_data <- Hitters[train,]
test_data <- Hitters[-train,]

library(gbm)
set.seed(103)
pows <- seq(-10, -2, 0.1)
lambdas <- 10 ^ pows
train_errors <- rep(NA, length(lambdas))
test_errors <- rep(NA, length(lambdas))
for(i in 1 : length(lambdas)){
  boost <- gbm(Salary ~ ., data = train_data, distribution = "gaussian", n.trees = 1000, 
               shrinkage = lambdas[i])
  train_pred <- predict(boost, train_data, n.trees = 1000)
  test_pred <- predict(boost, test_data, n.trees = 1000)
  train_errors[i] <- mean((train_pred - train_data$Salary)^2)
  test_errors[i] <- mean((test_pred - test_data$Salary)^2)
}
plot(lambdas, train_errors)
plot(lambdas, test_errors)
min(test_errors)

model_lm <- lm(Salary ~ ., train_data)
lm_pred <- predict(model_lm, test_data)
mean(lm_pred - test_data$Salary)

library(glmnet)
set.seed(134)
x <- model.matrix(Salary ~ ., data = train_data)
y <- train_data$Salary
x_test <- model.matrix(Salary ~ ., data = test_data)
model_lasso <- glmnet(x, y, alpha = 1)
lasso_pred <- predict(model_lasso, s = 0.01, newx = x_test)
mean((test_data$Salary - lasso_pred)^2)

boost_best <- gbm(Salary ~ ., data = train_data, distribution = "gaussian", n.trees = 1000, 
            shrinkage = lambdas[which.min(test_errors)])
summary(boost_best)

library(randomForest)

rf <- randomForest(Salary ~ ., train_data, ntree = 500, mtry = 19)
rf_pred <- predict(rf, test_data)
mean(rf_pred - test_data$Salary)

library(ISLR)
attach(Caravan)
train <- 1:1000
Caravan$Purchase <- ifelse(Caravan$Purchase == "Yes", 1, 0)
train_data <- Caravan[train,]
test_data <- Caravan[-train,]

library(gbm)
set.seed(1)
boost <- gbm(Purchase ~ ., data = train_data, shrinkage = 0.01, n.trees = 1000,
             distribution = "bernoulli")
summary(boost)

prob <- predict(boost, test_data, n.trees = 1000, type = "response")
pred <- ifelse(prob > 0.2, 1, 0)
table(pred, test_data$Purchase)

model_logi <- glm(Purchase ~ ., train_data, family = binomial)
logi_prob <- predict(model_logi, test_data, type = "response")
logi_pred <- ifelse(logi_prob > 0.2, 1, 0)
table(logi_pred, test_data$Purchase)
