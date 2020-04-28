library(ISLR)
fix(Hitters)

names(Hitters)
sum(is.na(Hitters$Salary))
Hitters = na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

library(leaps)
regfit.full = regsubsets(Salary ~ ., Hitters)
summary(regfit.full)

regfit.full = regsubsets(Salary ~ ., Hitters, nvmax = 19)
summary(regfit.full)
reg.summary = summary(regfit.full)
names(reg.summary)
reg.summary$rsq
plot(reg.summary$rsq)

par(mfrow = c(2,2))
plot(reg.summary$rsq)
plot(reg.summary$rss)
plot(reg.summary$adjr2)
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col = "red", cex = 2, pch = 20)

plot(reg.summary$cp)
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col = "red", cex = 2, pch = 20)

plot(reg.summary$bic)
which.min(reg.summary$bic)
points(6, reg.summary$bic[6], col = "red", cex = 2, pch = 20)

plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")
coef(regfit.full, 6)

regfit.fwd <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "forward")
summary(regfit.fwd)
regfit.bwd <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "backward")
summary(regfit.bwd)

coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(Hitters), rep = TRUE)
test <- !train
regfit.best <- regsubsets(Salary ~ .,data = Hitters[train,], nvmax = 19)
test.mat <- model.matrix(Salary~.,data = Hitters[test,])

val.errors <- rep(NA, 19)
for(i in 1:19){
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[,names(coefi)]%*%coefi
  val.errors[i] <- mean((Hitters$Salary[test]- pred) ^ 2)
}
val.errors
plot(val.errors)
which.min(val.errors)
coef(regfit.best, 7)

predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

regfit.best <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
coef(regfit.best, 10)

k <- 10
set.seed(1)
folds <- sample(1:k, nrow(Hitters), replace = TRUE)
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

for(j in 1:k){
  best.fit <- regsubsets(Salary ~ ., data = Hitters[folds != j,], nvmax = 19)
  for(i in 1:19){
    pred <- predict(best.fit, Hitters[folds == j , ], id = i)
    cv.errors[j,i] <- mean((Hitters$Salary[folds == j] - pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
plot(mean.cv.errors, type = "b")

reg.best <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
coef(reg.best, 11)


x <- model.matrix(Salary ~.,Hitters)[,-1]
y <- Hitters$Salary

library(glmnet)

grid <- 10 ^ seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lamdba = grid)
dim(coef(ridge.mod))

ridge.mod$lambda[50]
coef(ridge.mod)[,50]

ridge.mod$lambda[60]
coef(ridge.mod)[,60]

predict(ridge.mod, s = 50, type = "coefficients")[1:20,]

set.seed(1)
train <- sample(1:nrow(x),nrow(x)/2)
test <- -train
y.test <- y[test]

ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = 4, newx =x[test,])
mean((ridge.pred - y.test)^2)

mean((mean(y[train])-y.test)^2)

ridge.pred <- predict(ridge.mod, s = 1e10, newx =x[test,])
mean((ridge.pred - y.test)^2)

ridge.pred <- predict(ridge.mod, s = 0, newx =x[test,])
mean((ridge.pred - y.test)^2)

lm(y ~ x, subset = train)
predict(ridge.mod, s = 0, exact = TRUE, type = "coefficients")[1:20,]
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test,])
mean((ridge.pred - y.test)^2)
 
out <- glmnet(x ,y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20,]
lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.pred - y.test)^2)

out <- glmnet(x,y,alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = bestlam)[1:20,]
lasso.coef

library(pls)
Hitters = na.omit(Hitters)
set.seed(2)
pcr.fit <- pcr(Salary ~ ., data = Hitters, scale = TRUE, validation = "CV")

summary(pcr.fit)

validationplot(pcr.fit, val.type = "MSEP")

set.seed(1)
pcr.fit <- pcr(Salary ~ ., data = Hitters, subset = train, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")

pcr.pred <- predict(pcr.fit, x[test,], ncomp = 7)
mean((pcr.pred - y.test)^2)

pcr.fit <- pcr(y ~ x, scale = TRUE, ncomp = 7)
summary(pcr.fit)

set.seed(1)
pls.fit <- plsr(Salary ~ ., data = Hitters, subset = train, scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")
pls.pred <- predict(pls.fit, x[test,], ncomp = 2)
mean((pls.pred - y.test)^2)
pls.fit <- plsr(Salary ~ ., data = Hitters, scale = TRUE, ncomp = 2)
summary(pls.fit)

y <- 2
lambda <- 2
beta <- seq(-10,10,0.1)
L <- (1 + lambda)*beta^2 - 2*y*beta + y^2
plot(beta, L)
best <- which.min(L)
beta[best]
esti <- y/(1 + lambda)

y <- 2
lambda <- 2
beta <- seq(-10,10,0.1)
Lasso <- (y - beta)^2 + lambda * abs(beta)
plot(beta, Lasso)
best <- which.min(Lasso)
beta_best <- beta[best]
esti <- y - lambda/2

x <- rnorm(100)
eps <- rnorm(100)
beta0 <- 3
beta1 <- 2
beta2 <- -3
beta3 <- 0.3
Y <- beta0 + beta1*x + beta2*x^2 + beta3*x^3 + eps
library(leaps)
data <- data.frame(x, Y)
model <- regsubsets(Y ~ poly(x , 10, raw = TRUE), data = data, nvmax = 10)
model.summary <- summary(model)

which.min(model.summary$cp)
which.min(model.summary$bic)
which.max(model.summary$adjr2)

plot(model.summary$cp)
points(3,model.summary$cp[3],col = "red")

plot(model.summary$bic)
points(3,model.summary$bic[3],col = "red")

plot(model.summary$adjr2)
points(4,model.summary$adjr2[4],col = "red")

coefficients(model, id = 3)

model.fwd <- regsubsets(Y ~ poly(x , 10, raw = TRUE), data = data, nvmax = 10, method = "forward")
model.back <- regsubsets(Y ~ poly(x , 10, raw = TRUE), data = data, nvmax = 10, method = "backward")

model.summary.fwd <- summary(model.fwd)
model.summary.back <- summary(model.back)

which.min(model.summary.fwd$cp)
which.min(model.summary.fwd$bic)
which.max(model.summary.fwd$adjr2)

which.min(model.summary.back$cp)
which.min(model.summary.back$bic)
which.max(model.summary.back$adjr2)

coefficients(model.fwd, id = 3)
coefficients(model.back, id = 3)
coefficients(model, id = 6)

library(glmnet)
xmat = model.matrix(Y ~ poly(x, 10, raw = T), data = data)[, -1]
model.lasso <- cv.glmnet(xmat, Y, alpha = 1)
best_lambda <- model.lasso$lambda.min
plot(model.lasso)

predict(model.lasso, s = best_lambda, type = "coefficients")

beta7 <- 7
Y7 <- beta0 + beta7*x^7 + eps
data7 <- data.frame(x, Y7)
model_subset <- regsubsets(Y7 ~ poly(x , 10), data = data7, nvmax = 10)
subset_summary = summary(model_subset)

which.min(subset_summary$cp)
which.min(subset_summary$bic)
which.max(subset_summary$adjr2)

xmat = model.matrix(Y7 ~ poly(x, 10, raw = T), data = data7)[, -1]
model_lasso <- cv.glmnet(xmat, Y7, alpha = 1)
best_lambda <- model_lasso$lambda.min
predict(model_lasso, s = best_lambda, type = "coefficients")

library(ISLR)
set.seed(11)
sum(is.na(College))

div_size <- dim(College)[1]/2
train <- sample(1 : dim(College)[1], div_size)
test <- -train
train_data <- College[train,]
test_data <- College[test,]

lm_fit <- lm(Apps ~ ., train_data)
pred <- predict(lm_fit, test_data)
mean((pred - test_data[,"Apps"])^2)

train_mat <- model.matrix(Apps ~ ., data = train_data)
test_mat <- model.matrix(Apps ~ ., data = test_data)
lambdas <- 10 ^ seq(4, -2, length=100)
model_ridge <- cv.glmnet(train_mat, train_data[,"Apps"],alpha=0, lambda=lambdas, thresh=1e-12)
best_lambda <- model_ridge$lambda.min
ridge.pred = predict(model_ridge, newx=test_mat, s=best_lambda)
mean((test_data[, "Apps"] - ridge.pred)^2)

model_lasso <- cv.glmnet(train_mat, train_data[,"Apps"],alpha=1, lambda=lambdas, thresh=1e-12)
best_lambda_lasso <- model_lasso$lambda.min
lasso.pred = predict(model_lasso, newx=test_mat, s=best_lambda_lasso)
mean((test_data[, "Apps"] - lasso.pred)^2)

model_lasso_all <- glmnet(model.matrix(Apps~., data=College), College[, "Apps"], alpha=1)
predict(model_lasso_all, s=best_lambda_lasso, type="coefficients")

library(pls)
model_pcr <- pcr(Apps~., data=train_data, scale=T, validation="CV")
validationplot(model_pcr, val.type = "MSEP")
pcr.pred = predict(model_pcr, test_data, ncomp=10)
mean((test_data[, "Apps"] - data.frame(pcr.pred))^2)

model_plsr <- plsr(Apps~., data=train_data, scale=T, validation="CV")
validationplot(model_plsr)
plsr.pred = predict(model_plsr, test_data, ncomp=10)
mean((test_data[, "Apps"] - pcr.pred)^2)

set.seed(1)
p <- 20
n <- 1000
x <- matrix(rnorm(n * p), n, p)
beta <- rnorm(p)
beta[3] <- 0
beta[4] <- 0
beta[9] <- 0
beta[19] <- 0
beta[10] <- 0
eps <- rnorm(p)
Y <- x%*%beta + eps
train <- sample(seq(n), 100, replace = FALSE)
train_Y <- Y[train,]
train_X <- x[train,]
test_Y <- Y[-train,]
test_X <- x[-train,]

library(leaps)
model_subset <- regsubsets(Y ~ ., data = data.frame(x = train_X, Y = train_Y), nvmax = p)
errors <- rep(0, p)
x_columns <- colnames(x, do.NULL = FALSE, prefix = "x.")
for (i in 1:p) {
  coefi <- coef(model_subset, id = i)
  pred <- as.matrix(train_X[, x_columns %in% names(coefi)]) %*% coefi[names(coefi) %in% 
                                                                       x_columns]
  errors[i] <- mean((train_Y - pred)^2)
}
plot(errors, ylab = "Training MSE", pch = 19, type = "b")

errors_test <- rep(0, p)
x_columns <- colnames(x, do.NULL = FALSE, prefix = "x.")
for (i in 1:p) {
  coefi <- coef(model_subset, id = i)
  pred <- as.matrix(test_X[, x_columns %in% names(coefi)]) %*% coefi[names(coefi) %in% 
                                                                        x_columns]
  errors_test[i] <- mean((test_Y - pred)^2)
}

plot(errors_test, ylab = "Test MSE", pch = 19, type = "b")
which.min(errors_test)
coef(model_subset, id = 18)

val.errors = rep(NA, p)
a = rep(NA, p)
b = rep(NA, p)
for (i in 1:p) {
  coefi = coef(model_subset, id = i)
  a[i] = length(coefi) - 1
  b[i] = sqrt(sum((beta[x_columns %in% names(coefi)] - coefi[names(coefi) %in% x_columns])^2) + 
                sum(beta[!(x_columns %in% names(coefi))])^2)
}
plot(x = a, y = b)
plot(x = a, y = b, xlab = "number of coefficients", ylab = "error between estimated and true coefficients")