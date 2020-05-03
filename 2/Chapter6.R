library(ISLR)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

library(leaps)
regfit_full <- regsubsets(Salary ~ ., Hitters)
summary(regfit_full)

regfit_full <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
summary(regfit_full)

names(summary(regfit_full))

summary(regfit_full)$rsq
par(mfrow = c(2,2))
plot(summary(regfit_full)$rss, type = "l")
plot(summary(regfit_full)$adjr2, type = "l")

which.max(summary(regfit_full)$adjr2)
points(11, summary(regfit_full)$adjr2[11], col = "red")

plot(summary(regfit_full)$cp, type = "l")
which.min(summary(regfit_full)$cp)
points(10, summary(regfit_full)$cp[10], col = "red")

plot(summary(regfit_full)$bic, type = "l")
which.min(summary(regfit_full)$bic)
points(6, summary(regfit_full)$bic[6], col = "red")

plot(regfit_full, scale = "r2")
plot(regfit_full, scale = "adjr2")
plot(regfit_full, scale = "Cp")
plot(regfit_full, scale = "bic")

coef(regfit_full, 6)

regfit_fwd <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "forward")
summary(regfit_fwd)

regfit_bwd <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "backward")
summary(regfit_bwd)

coef(regfit_full,7)
coef(regfit_fwd,7)
coef(regfit_bwd,7)

set.seed(1)
train <- sample(c(T, F), nrow(Hitters), rep = T)
test <- !train

regfit_best <- regsubsets(Salary ~ ., data = Hitters[train,], nvmax = 19)
test_mat <- model.matrix(Salary ~ ., data = Hitters[test,])

errors <- rep(NA, 19)
for(i in 1:19){
  coefi <- coef(regfit_best, id = i)
  pred <- test_mat[,names(coefi)]%*%coefi
  errors[i] <- mean((Hitters$Salary[test] - pred)^2)
}
errors
which.min(errors)
plot(errors)
coef(regfit_best, 7)

predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

regfit_best_all <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
coef(regfit_best_all, 7)

k <- 10
folds <- sample(1:k, nrow(Hitters), replace = T)
cv_errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1: 19)))

for(j in 1:k){
  best_fit <- regsubsets(Salary ~ ., data = Hitters[folds != j,], nvmax = 19)
  for(i in 1:19){
    pred <- predict(best_fit, Hitters[folds == j,], id = i)
    cv_errors[j,i] <- mean((Hitters $ Salary[folds == j] - pred)^2)
  }
}
mean_cv_errors <- apply(cv_errors, 2, mean)
mean_cv_errors
plot(mean_cv_errors)
which.min(mean_cv_errors)

reg_best <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
coef(reg_best, 11)

x <- model.matrix(Salary ~ ., Hitters)[, -1]
y <- Hitters$Salary
y <- na.omit(y)

library(glmnet)
grid <- 10 ^ seq(10, -2, length = 100)
model_ridge <- glmnet(x, y, alpha = 0, lambda = grid)
dim(coef(model_ridge))

model_ridge$lambda[50]
coef(model_ridge)[,50]
sqrt(sum(coef(model_ridge)[-1, 50] ^ 2))

model_ridge$lambda[60]
coef(model_ridge)[,60]
sqrt(sum(coef(model_ridge)[-1, 60] ^ 2))

predict(model_ridge, s = 50, type = "coefficients")[1:20,]

set.seed(1)
train <- sample(1: nrow(x), nrow(x)/2)
test <- -train
y_test <- y[test]

model_ridge <- glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12) 
pred <- predict(model_ridge, s = 4, newx = x[test,])
mean((pred - y_test)^2)

mean((mean(y[train]) - y_test)^2)

pred <- predict(model_ridge, s = 1e10, newx = x[test,])
mean((pred - y_test)^2)

pred <- predict(model_ridge, s = 0, newx = x[test,])
mean((pred - y_test)^2)

lm(y ~ x, subset = train)
predict(model_ridge, s = 0, type = "coefficients")[1:20,]

cv <- cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv)
bestlam <- cv$lambda.min
bestlam

pred <- predict(model_ridge, s = bestlam, newx = x[test,])
mean((pred - y_test)^2)

out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20,]

model_lasso <- glmnet(x[train,], y[train], alpha = 1, lambda = grid)
plot(model_lasso)

cv <- cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv)
bestlam <- cv$lambda.min
pred <- predict(model_lasso, s = bestlam, newx = x[test,])
mean((pred - y_test)^2)

out <- glmnet(x, y, alpha = 1, lambda = grid)
coef <- predict(out, type = "coefficients", s = bestlam)[1:20,]
coef

library(pls)
set.seed(2)
Hitters <- na.omit(Hitters)
model_pcr <- pcr(Salary ~ ., data = Hitters, scale = T, validation = "CV")
summary(model_pcr)

validationplot(model_pcr, val.type = "MSEP")

set.seed(1)
model_pcr <- pcr(Salary ~ ., data = Hitters, subset = train, scale = T, validation = "CV")
validationplot(model_pcr, val.type = "MSEP")
pred <- predict(model_pcr, x[test,],ncomp = 7)
mean((pred - y_test)^2)

model_pcr <- pcr(y ~ x, scale = T, ncomp = 7)
summary(model_pcr)

set.seed(1)
model_pls <- plsr(Salary ~ ., data = Hitters, subset = train, scale = T, validation = "CV")
summary(model_pls)

validationplot(model_pls, val.type = "MSEP")

pred <- predict(model_pls, x[test,], ncomp = 2)
mean((pred - y_test)^2)

model_pls <- plsr(Salary ~ ., data = Hitters, scale = T, ncomp = 2)
summary(model_pls)

set.seed(1)
X <- rnorm(100)
eps <- rnorm(100)

beta0 <- 3
beta1 <- 2
beta2 <- -3
beta3 <- 0.3
Y <- beta0 + beta1 * X + beta2 * X^2 + beta3 * X^3 + eps

library(leaps)
data_full <- data.frame(y = Y, x = X)
model_best <- regsubsets(y ~ poly(x, 10), data = data_full, nvmax = 10)
model_best_summary <- summary(model_best)
model_best_summary$cp
model_best_summary$bic
model_best_summary$adjr2

which.min(model_best_summary$cp)
which.min(model_best_summary$bic)
which.max(model_best_summary$adjr2)

plot(model_best_summary$cp)
plot(model_best_summary$bic)
plot(model_best_summary$adjr2)

coefficients(model_best, id = 4)

model_fwd <- regsubsets(y ~ poly(x, 10), data = data_full, nvmax = 10, method = "forward")
model_fwd_summary <- summary(model_fwd)
model_fwd_summary$cp
model_fwd_summary$bic
model_fwd_summary$adjr2

which.min(model_fwd_summary$cp)
which.min(model_fwd_summary$bic)
which.max(model_fwd_summary$adjr2)

plot(model_fwd_summary$cp)
plot(model_fwd_summary$bic)
plot(model_fwd_summary$adjr2)

coefficients(model_fwd, id = 4)

model_bwd <- regsubsets(y ~ poly(x, 10), data = data_full, nvmax = 10, method = "backward")
model_bwd_summary <- summary(model_bwd)
model_bwd_summary$cp
model_bwd_summary$bic
model_bwd_summary$adjr2

which.min(model_bwd_summary$cp)
which.min(model_bwd_summary$bic)
which.max(model_bwd_summary$adjr2)

plot(model_bwd_summary$cp)
plot(model_bwd_summary$bic)
plot(model_bwd_summary$adjr2)

coefficients(model_bwd, id = 4)

library(glmnet)
mat <- model.matrix(y ~ poly(x, 10), row = T, data = data_full)[, -1]

model_lasso <- cv.glmnet(mat, Y, alpha = 1)
best_lambda <- model_lasso$lambda.min
best_lambda

plot(model_lasso)

model_lasso_best <- glmnet(mat, Y, alpha = 1)
predict(model_lasso_best, s = best_lambda, type = "coefficients")

beta7 <- 7
Y <- beta0 + beta7 * X^7 + eps
data_full <- data.frame(y = Y, x = X)
odel_best <- regsubsets(y ~ poly(x, 10), data = data_full, nvmax = 10)
model_best_summary <- summary(model_best)
model_best_summary$cp
model_best_summary$bic
model_best_summary$adjr2

which.min(model_best_summary$cp)
which.min(model_best_summary$bic)
which.max(model_best_summary$adjr2)

plot(model_best_summary$cp)
plot(model_best_summary$bic)
plot(model_best_summary$adjr2)

coefficients(model_best, id = 4)

mat <- model.matrix(y ~ poly(x, 10), row = T, data = data_full)[, -1]

model_lasso <- cv.glmnet(mat, Y, alpha = 1)
best_lambda <- model_lasso$lambda.min
best_lambda

plot(model_lasso)

model_lasso_best <- glmnet(mat, Y, alpha = 1)
predict(model_lasso_best, s = best_lambda, type = "coefficients")

library(ISLR)
attach(College)

set.seed(11)
sum(is.na(College))

train <- sample(1:dim(College)[1], dim(College)[1]/2)
test <- -train
train_data <- College[train,]
test_data <- College[test,]

model_lm <- lm(Apps ~ ., data = train_data)
pred_lm <- predict(model_lm, test_data)
mean((test_data[,"Apps"] - pred_lm)^2)

train_mat <- model.matrix(Apps ~ ., data = train_data)
test_mat <- model.matrix(Apps ~ ., data = test_data)
grid <- 10 ^ seq(4, -2, length=100)
model_rigde <- cv.glmnet(train_mat, train_data[, "Apps"], lambda = grid, alpha= 0,thresh=1e-12)
lambda_best_ridge <- model_rigde$lambda.min
lambda_best_ridge
pred_ridge <- predict(model_rigde, newx=test_mat, s=lambda_best_ridge)
mean((test_data[,"Apps"] - pred_ridge)^2)

model_lasso <- cv.glmnet(train_mat, train_data[, "Apps"], lambda = grid, alpha = 1,thresh=1e-12)
lambda_best_lasso <- model_lasso$lambda.min
lambda_best_lasso
pred_lasso <- predict(model_lasso, newx=test_mat, s=lambda_best_lasso)
mean((test_data[,"Apps"] - pred_lasso)^2)

model_lasso <- glmnet(train_mat, train_data[, "Apps"], alpha = 1)
predict(model_lasso, s = lambda_best_lasso,type="coefficients")

library(pls)

model_pls <- pcr(Apps ~ ., data = train_data, scale = T, validation = "CV")
validationplot(model_pls,val.type="MSEP")
pred_pcr <- predict(model_pls, test_data)
mean((test_data[,"Apps"] - pred_pcr)^2)

model_pls <- plsr(Apps ~ ., data = train_data, scale = T, validation = "CV")
validationplot(model_pls,val.type="MSEP")
pred_pcr <- predict(model_pls, test_data)
mean((test_data[,"Apps"] - pred_pcr)^2)

set.seed(1)
p <- 20
n <- 1000
x <- matrix(rnorm(n * p), n, p)
B <- rnorm(p)
B[3] <- 0
B[4] <- 0
B[9] <- 0
B[19] <- 0
B[10] <- 0
eps <- rnorm(p)
y <- x %*% B + eps

train <- sample(seq(1000), 100, replace = F)



x_train <- x[train,]
x_test <- x[-train, ]
y_train <- y[train, ]
y_test <- y[-train, ]

library(leaps)
model_best <- regsubsets(y ~ ., data = data.frame(x = x_train, y = y_train), nvmax = p)
errors <- rep(NA, p)
x_cols <- colnames(x, do.NULL = FALSE, prefix = "x.")
for (i in 1:p) {
  coefi <- coef(model_best, id = i)
  pred <- as.matrix(x_train[, x_cols %in% names(coefi)]) %*% coefi[names(coefi) %in% 
                                                                    x_cols]
  errors[i] <- mean((y_train - pred)^2)
}
plot(errors, ylab = "Training MSE", pch = 19, type = "b")

errors <- rep(NA, p)
x_cols <- colnames(x, do.NULL = FALSE, prefix = "x.")
for (i in 1:p) {
  coefi <- coef(model_best, id = i)
  pred <- as.matrix(x_test[, x_cols %in% names(coefi)]) %*% coefi[names(coefi) %in% 
                                                                     x_cols]
  errors[i] <- mean((y_test - pred)^2)
}
plot(errors, ylab = "Training MSE", pch = 19, type = "b")

which.min(errors)
coef(model_best, id = 18)

errors = rep(NA, p)
a = rep(NA, p)
b = rep(NA, p)
for (i in 1:p) {
  coefi = coef(model_best, id = i)
  a[i] = length(coefi) - 1
  b[i] = sqrt(sum((B[x_cols %in% names(coefi)] - coefi[names(coefi) %in% x_cols])^2) + 
                sum(B[!(x_cols %in% names(coefi))])^2)
}
plot(x = a, y = b, xlab = "number of coefficients", 
     ylab = "error between estimated and true coefficients")
which.min(b)
