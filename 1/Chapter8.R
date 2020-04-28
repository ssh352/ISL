library(tree)
library(ISLR)
attach(Carseats)
High <- ifelse(Sales <= 8, "No", "Yes")
Carseats <- data.frame(Carseats, High)

tree_Carseats <- tree(High ~ .-Sales, Carseats)
summary(tree_Carseats)

plot(tree_Carseats)
text(tree_Carseats, pretty = 0)

tree_Carseats

set.seed(2)
train <- sample(1:nrow(Carseats), 200)
test_data <- Carseats[-train,]
test_high <- High[-train]
model <- tree(High ~.-Sales, Carseats, subset = train)
pred <- predict(model, test_data, type = "class")
table(pred, test_high)
(104+50)/200

set.seed(3)
model_cv <- cv.tree(model, FUN = prune.misclass)
names(model_cv)
model_cv
par(mfrow = c(1,2))
plot(model_cv$size, model_cv$dev)
plot(model_cv$size, model_cv$dev)

model_prune <- prune.misclass(model, best = 9)
plot(model_prune)
text(model_prune, pretty = 0)

pred_tree <- predict(model_prune, test_data, type = "class")
table(pred_tree, test_high)
(97+58)/200

model_prune <- prune.misclass(model, best = 13)
pred_tree <- predict(model_prune, test_data, type = "class")
table(pred_tree, test_high)
(102+53)/200

library(MASS)
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
model_Boston <- tree(medv ~ ., Boston, subset = train)
summary(model_Boston)
plot(model_Boston)
text(model_Boston, pretty = 0)

cv_Boston <- cv.tree(model_Boston)
plot(cv_Boston$size, cv_Boston$dev)
prune_Boston <- prune.tree(model_Boston, best = 5)
plot(prune_Boston)
text(prune_Boston, pretty = 0)

yhat <- predict(model_Boston, newdata = Boston[-train,])
test_Boston <- Boston[-train, "medv"]
plot(yhat, test_Boston)
mean((yhat - test_Boston)^2)

library(randomForest)
set.seed(1)
bag_Boston <- randomForest(medv ~., data = Boston, subset = train, mtry = 13, importance = TRUE)
bag_Boston
plot(bag_Boston)
yhat_bag <- predict(bag_Boston, newdata <- Boston[-train,])
plot(yhat_bag , test_Boston)
abline(0,1)
mean((yhat_bag - test_Boston)^2)

bag_Boston <- randomForest(medv ~., data = Boston, subset = train, mtry = 13, ntree = 25)
yhat_bag <- predict(bag_Boston, newdata <- Boston[-train,])
plot(yhat_bag , test_Boston)
abline(0,1)
mean((yhat_bag - test_Boston)^2)

bag_Boston <- randomForest(medv ~., data = Boston, subset = train, mtry = 6, importance = TRUE)
yhat_bag <- predict(bag_Boston, newdata <- Boston[-train,])
plot(yhat_bag , test_Boston)
abline(0,1)
mean((yhat_bag - test_Boston)^2)
importance(bag_Boston)

varImpPlot(bag_Boston)

library(gbm)
set.seed(1)
boost_Boston <- gbm(medv ~ ., data = Boston[train,], distribution = "gaussian", n.trees = 5000)
summary(boost_Boston)

par(mfrow = c(1,2))
plot(boost_Boston, i = "rm")
plot(boost_Boston, i = "lstat")

yhat_boost <- predict(boost_Boston, newdata = Boston[-train,], n.trees = 5000)
mean((yhat_boost - test_Boston)^2)

boost_Boston <- gbm(medv ~ ., data = Boston[train,], distribution = "gaussian", n.trees = 5000, shrinkage = 0.2, verbose = F)
yhat_boost <- predict(boost_Boston, newdata = Boston[-train,], n.trees = 5000)
mean((yhat_boost - test_Boston)^2)

p <- seq(0,1,0.001)
gini = p * (1 - p) * 2
entropy = -(p * log(p) + (1 - p) * log(1 - p))
error = 1 - pmax(p, 1 - p)
matplot(p, cbind(gini, entropy, error), col = c("red", "green", "blue"))

library(MASS)
attach(Boston)
library(randomForest)

set.seed(1101)

summary(Boston)
train <- sample(dim(Boston)[1], dim(Boston)[1]/2)
train_X <- Boston[train, -14]
train_Y <- Boston[train, 14]
test_X <- Boston[-train, -14]
test_Y <- Boston[-train, 14]

p <- dim(Boston)[2] - 1
p2 <- p/2
p_sqrt <- sqrt(p)


rf_p = randomForest(train_X, train_Y, xtest = test_X, ytest = test_Y, mtry = p, ntree = 500)
rf_p2 = randomForest(train_X, train_Y, xtest = test_X, ytest = test_Y, mtry = p2, ntree = 500)
rf_psqrt = randomForest(train_X, train_Y, xtest = test_X, ytest = test_Y, mtry = p_sqrt, ntree = 500)
plot(1:500, rf_p$test$mse, type = "l")
lines(1:500, rf_p2$test$mse, col = "red")
lines(1:500, rf_psqrt$test$mse, col = "blue")

library(ISLR)
attach(Carseats)
set.seed(1)

train <- sample(dim(Carseats)[1], dim(Carseats)[1]/2)
train_data <- Carseats[train,]
test_data <- Carseats[-train,]

library(tree)
tree <- tree(Sales ~ ., data = train_data)
plot(tree)
text(tree, pretty = 0, cex = 0.5)

pred <- predict(tree, test_data)
mean((test_data$Sales - pred)^2)

CV_tree <- cv.tree(tree, FUN = prune.tree)
plot(CV_tree$size, CV_tree$dev)
plot(CV_tree$k, CV_tree$dev)
which.min(CV_tree$dev)

prune_tree <- prune.tree(tree, best = 9)
plot(prune_tree)
text(prune_tree, pretty = 0)

pred <- predict(prune_tree, test_data)
mean((test_data$Sales - pred)^2)

bag <- randomForest(Sales ~ ., data = train_data, mtry = 10, ntree = 500, importance = T)
pred <- predict(bag, test_data)
mean((test_data$Sales - pred)^2)
importance(bag)

library(ISLR)
attach(OJ)
set.seed(1013)

train <- sample(dim(OJ)[1], 800)
train_data <- OJ[train,]
test_data <- OJ[-train,]

library(tree)
model_tree <- tree(Purchase ~ ., data = train_data)
summary(model_tree)

model_tree

plot(model_tree)
text(model_tree, pretty = 0, cex = 0.5)

pred <- predict(model_tree, test_data, type = "class")
table(test_data$Purchase, pred)

model_CV <- cv.tree(model_tree, FUN = prune.tree)

plot(model_CV$size, model_CV$dev)
model_CV$dev
model_CV$size

model_pruned <- prune.tree(model_tree, best = 8)
summary(model_pruned)

pred_pruned <- predict(model_pruned, test_data, type = "class")
table(test_data$Purchase, pred_pruned)

library(ISLR)
attach(Hitters)
Hitters <- Hitters[-which(is.na(Hitters$Salary)), ]
sum(is.na(Hitters$Salary))

Hitters$Salary <- log(Hitters$Salary)
train <- 1:200
train_data <- Hitters[train,]
test_data <- Hitters[-train,]

library(gbm)
set.seed(103)
lambdas <- 10 ^ seq(-10, -0.2, by = 0.1)
len_lambdas <- length(lambdas)
train_errors <- rep(NA, len_lambdas)
test_errors <- rep(NA, len_lambdas)
for(i in 1:len_lambdas){
  model_boost <- gbm(Salary ~., data = train_data, distribution = "gaussian", n.trees = 1000, shrinkage = lambdas[i])
  train_pred <- predict(model_boost, train_data, n.trees = 1000)
  test_pred <- predict(model_boost, test_data, n.trees = 1000)
  train_errors[i] <- mean((train_data$Salary -  train_pred)^2)
  test_errors[i] <- mean((test_data$Salary - test_pred)^2)
}
plot(lambdas, train_errors)
plot(lambdas, test_errors)
min(test_errors)

library(glmnet)
set.seed(134)
x <- model.matrix(Salary ~ ., data = train_data)
y <- train_data$Salary
x_test <- model.matrix(Salary ~ ., data = test_data)
lasso <- glmnet(x, y, alpha = 1)
lasso_pred <- predict(lasso, s = 0.01, newx = x_test)
mean((test_data$Salary - lasso_pred)^2)

boost_best <- gbm(Salary ~., data = train_data, distribution = "gaussian", n.trees = 1000, shrinkage = lambdas[which.min(test_errors)]) 
summary(boost_best)

library(randomForest)
set.seed(21)
model_RM <- randomForest(Salary ~., data = train_data, ntree = 500, mtry = 19)
pred_RM <- predict(model_RM, test_data)
mean((test_data$Salary - pred_RM)^2)

library(ISLR)
Caravan$Purchase <- ifelse(Caravan$Purchase == "Yes", 1,0)
train_data <- Caravan[1:1000,]
test_data <- Caravan[-(1:1000),]
library(gbm)
set.seed(342)
boost <- gbm(Purchase ~., data = train_data, n.trees = 1000, shrinkage = 0.1, distribution = "bernoulli")
summary(boost)

prob <- predict(boost, test_data, n.trees = 1000, type = "response")
pred <- ifelse(prob > 0.2, 1,0)
table(test_data$Purchase, pred)
47/(339 + 47)

lm <- glm(Purchase ~., data = train_data, family = binomial)
lm_prob <- predict(lm, test_data, type = "response")
lm_pred <- ifelse(lm_prob > 0.2 , 1, 0)
table(test_data$Purchase, lm_pred)
58/(350 + 58)
