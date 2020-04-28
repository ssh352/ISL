library(ISLR)
attach(Wage)

fit <- lm(wage ~ poly(age, 4), data = Wage)
coef(summary(fit))

fit2 <- lm(wage ~ poly(age, 4, raw = T), data = Wage)
coef(summary(fit2))

fit2a <- lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data = Wage)
coef(fit2a)

fit2b <- lm(wage ~ cbind(age, age^2, age^3, age^4), data = Wage)

agelims <- range(age)
age.grid <- seq(from = agelims[1], to = agelims[2])
preds <- predict(fit, newdata = list(age = age.grid),se = TRUE)
se.bands <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)

par(mfrow = c(1,2), mar=c(4.5, 4.5, 1,1), oma = c(0,0,4,0))
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("4次多項式", outer = T)
lines(age.grid, preds$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

preds2 <- predict(fit2, newdata = list(age = age.grid), se = TRUE)
max(abs(preds$fit - preds2$fit))

fit.1 <- lm(wage ~ age, data = Wage)
fit.2 <- lm(wage ~ poly(age,2), data = Wage)
fit.3 <- lm(wage ~ poly(age,3), data = Wage)
fit.4 <- lm(wage ~ poly(age,4), data = Wage)
fit.5 <- lm(wage ~ poly(age,5), data = Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)
coef(summary(fit.5))

(11.983)^2

fit.1 <- lm(wage ~ education + age, data = Wage)
fit.2 <- lm(wage ~ education + poly(age,2), data = Wage)
fit.3 <- lm(wage ~ education + poly(age, 3), data = Wage)
anova(fit.1, fit.2, fit.3)

fit <- glm(I(wage > 250) ~ poly(age, 4), data = Wage, family = binomial)
preds <- predict(fit, newdata = list(age = age.grid), se = T)

pfit <- exp(preds$fit)/(1 + exp(preds$fit))
se.bands.logit <- cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
se.bands <- exp(se.bands.logit)/(1 + exp(se.bands.logit))

preds <- predict(fit, newdata = list(age = age.grid), type = "response", se = T)

plot(age, I(wage > 250), xlim = agelims, type = "n", ylim = c(0,.2))
points(jitter(age), I((wage > 250)/5), cex = .5, pch = "|", col = "darkgrey")
lines(age.grid, pfit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

table(cut(age, 4))
fit <- lm(wage ~ cut(age, 4), data = Wage)
coef(summary(fit))

library(splines)
fit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)
pred <- predict(fit, newdata = list(age = age.grid), se = T)
plot(age, wage, col = "gray")
lines(age.grid, pred$fit, lwd = 2)
lines(age.grid, pred$fit + 2 * pred$se, lty = "dashed")
lines(age.grid, pred$fit - 2 * pred$se, lty = "dashed")

dim(bs(age, knots = c(25, 40, 60)))
dim(bs(age, df = 6))
attr(bs(age, df = 6), "knots")

fit2 <- lm(wage ~ ns(age, df = 4), data = Wage)
pred2 <- predict(fit2, newdata = list(age = age.grid), se = T)
lines(age.grid, pred2$fit, col = "red", lwd = 2)

plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
fit <- smooth.spline(age, wage, df = 16)
fit2 <- smooth.spline(age, wage, cv = TRUE)
fit2$df
lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)

plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
fit <- loess(wage ~ age, span= .2, data = Wage)
fit2 <- loess(wage ~ age, span = .5, data = Wage)
lines(age.grid, predict(fit, data.frame(age = age.grid)), col = "red", lwd = 2)
lines(age.grid, predict(fit2, data.frame(age = age.grid)), col = "blue", lwd = 2)

gam1 <- lm(wage ~ ns(year, 4) + ns(age, 5) + education, data = Wage)

library(gam)
gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)

par(mfrow = c(1,3))
plot(gam.m3, se = TRUE, col = "blue")
plot.gam(gam1, se = TRUE, col = "red")

gam.m1 <- gam(wage ~ s(age, 5) + education, data = Wage)
gam.m2 <- gam(wage ~ year + s(age, 5)+ education, data = Wage)
anova(gam.m1, gam.m2, gam.m3, test = "F")
summary(gam.m3)

preds <- predict(gam.m2, newdata=Wage)
gam.lo <- gam(wage ~ s(year, df = 4) + lo(age, span = 0.7) + education, data = Wage)
plot.Gam(gam.lo, se = TRUE, col = "green")

gam.lo.i <- gam(wage ~ lo(year, age, span = 0.5) + education, data = Wage)

library(akima)
plot(gam.lo.i)

gam.lr <- gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial, data = Wage)
par(mfrow = c(1,3))
plot(gam.lr, se = T, col = "green")
table(education, I(wage > 250))
gam.lr.s <- gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial, data = Wage, subset = (education != "1. < HS Grad"))
plot(gam.lr.s, se = T, col = "green")

library(ISLR)
attach(Wage)
library(boot)
delta = rep(NA, 10)
for(n in 1:10){
  model <- glm(wage ~ poly(age, n), data = Wage)
  delta[n] <- cv.glm(Wage, model, K=10)$delta[2]
}
min_point <- which.min(delta)

model_1 <- lm(wage ~ poly(age, 1), data = Wage)
model_2 <- lm(wage ~ poly(age, 2), data = Wage)
model_3 <- lm(wage ~ poly(age, 3), data = Wage)
model_4 <- lm(wage ~ poly(age, 4), data = Wage)
model_5 <- lm(wage ~ poly(age, 5), data = Wage)
model_6 <- lm(wage ~ poly(age, 6), data = Wage)
model_7 <- lm(wage ~ poly(age, 7), data = Wage)
model_8 <- lm(wage ~ poly(age, 8), data = Wage)
model_9 <- lm(wage ~ poly(age, 9), data = Wage)
model_10 <- lm(wage ~ poly(age, 10), data = Wage)
anova(model_1, model_2, model_3, model_4, model_5, model_6, model_7, model_8, model_9, model_10)

cvs <- rep(NA, 10)
for(i in 2:10){
  Wage$age.cut <- cut(Wage$age, i)
  lm.fit <- glm(wage ~ age.cut, data = Wage)
  cvs[i] <- cv.glm(Wage, lm.fit, K = 10)$delta[2]
}
plot(2:10, cvs[-1])

lm.fit <- glm(wage ~ cut(age,8), data = Wage)
agelims <- range(Wage$age)
age.grid <- seq(from = agelims[1], to = agelims[2])
lm.pred <- predict(lm.fit, data.frame(age = age.grid))
plot(wage ~ age, data = Wage)
lines(age.grid, lm.pred, col = "red", lwd = 2)

set.seed(1)
summary(Wage$maritl)
summary(Wage$jobclass)

plot(Wage$maritl, Wage$wage)
plot(Wage$jobclass, Wage$wage)

model <- lm(wage ~ maritl, data = Wage)
deviance(model)

model<- lm(wage ~ jobclass, data = Wage)
deviance(model)

model <- lm(wage ~ maritl + jobclass, data = Wage)
deviance(model)

library(gam)
fit <- gam(wage ~ maritl + jobclass + s(age, 4), data = Wage)
deviance(fit)

library(ISLR)
attach(Auto)
set.seed(1)

RSS <- rep(NA, 10)
fits <- list()
for(d in 1:10){
  fits[[d]] <- lm(mpg ~ poly(displacement, d), data = Auto)
  RSS[d] <- deviance(fits[[d]])
}
plot(RSS)

anova(fits[[1]],fits[[2]],fits[[3]],fits[[4]],fits[[5]],fits[[6]])

library(glmnet)
library(boot)

errors <- rep(NA, 15)
for(d in 1:15){
  fit <- glm(mpg ~ poly(displacement, d), data = Auto)
  errors[d] <- cv.glm(Auto, fit, K = 10)$delta[2]
}
plot(errors)
which.min(errors)

errors <- rep(NA, 10)
for(c in 2:10){
  Auto$dis.cut <- cut(Auto$displacement, c)
  fit <- glm(mpg ~ dis.cut, data = Auto)
  errors[c] <- cv.glm(Auto, fit, K = 10)$delta[2]
}
plot(errors)
which.min(errors)

library(gam)
fit <- gam(mpg ~ s(displacement, 4) + s(horsepower, 4), data = Auto)
summary(fit)

library(MASS)
attach(Boston)

model_a <- lm(nox ~ poly(dis, 3), data=Boston)
summary(model_a)

dis_range <- range(dis)
dis_grid <- seq(from = dis_range[1], to = dis_range[2], by = 0.1)
lm.pred <- predict(model_a, list(dis = dis_grid))
plot(nox ~ dis, data = Boston)
lines(dis_grid, lm.pred, col = "red")

RSS <- rep(NA, 10)
for(i in 1:10){
  lm.fit <- lm(nox ~ poly(dis,i), data = Boston)
  RSS[i] <- sum(lm.fit$residuals^2)
}
plot(RSS)

library(boot)
deltas <- rep(NA , 10)
for(i in 1:10){
  model <- glm(nox ~ poly(dis, i), data = Boston)
  deltas[i] <- cv.glm(Boston, model, K = 10)$delta[2]
}
plot(1 :10 , deltas)

library(splines)
model_sp <- lm(nox ~ bs(dis, df = 4, knots = c(4,7,11)), data = Boston)
summary(model_sp)
pred_sp <- predict(model_sp, list(dis = dis_grid))
plot(nox ~ dis, data = Boston)
lines(dis_grid, pred_sp, col = "red")

CV <- rep(NA, 16)
for(i in 3:16){
  model <- lm(nox ~ bs(dis, df = i), data = Boston)
  CV[i] <- sum(model$residuals^2)
}
plot(3:16, CV[-c(1,2)])

CV <- rep(NA, 16)
for(i in 3:16){
  model <- glm(nox ~ bs(dis, df = i), data = Boston)
  CV[i] <- cv.glm(Boston, model, K = 10)$delta[2]
}
plot(3:16, CV[-c(1,2)])

library(ISLR)
library(leaps)
attach(College)

train <- sample(length(Outstate), length(Outstate)/2)
test <- -train
train_data <- College[train,]
test_data <- College[-train,]
model <- regsubsets(Outstate ~., data = train_data, nvmax = 17, method = "forward")
model_summary <- summary(model)
plot(model_summary$cp)
plot(model_summary$bic)
plot(model_summary$adjr2)
best_cp <- which.min(model_summary$cp)
best_bic <- which.min(model_summary$bic)
best_adjr <- which.max(model_summary$adjr2)

coefi <- coef(model, id = 6)
coefi

library(gam)

gam_model <- gam(Outstate ~ Private + s(Room.Board, df = 2) + s(PhD, df = 2) +
  s(perc.alumni, df = 2) + s(Expend, df = 5) + s(Grad.Rate, df = 2), data = train_data)
par(mfrow = c(2,3))
plot(gam_model, se = T)

gam_pred <- predict(gam_model, test_data)
error <- mean((test_data$Outstate - gam_pred)^2)
error

gan_tss <- mean((test_data$Outstate - mean(test_data$Outstate))^2)
tss <- 1 - error/gan_tss

summary(gam_model)

set.seed(1)
X1 <- rnorm(100)
X2 <- rnorm(100)
eps <- rnorm(100, sd = 0.1)
Y <- -2.1 + 1.3 * X1 + 0.54 * X2 + eps

beta0 <- rep(NA, 1000)
beta1 <- rep(NA, 1000)
beta2 <- rep(NA, 1000)
beta1[1] <- 10

for(i in 1:1000){
  a <- Y - beta1[i] * X1
  beta2[i] <- lm(a ~ X2)$coef[2]
  a <- Y - beta2[i] * X2
  if( i < 1000){
    beta1[i + 1] <- lm(a ~ X1)$coef[2]  
  }
  beta0[i] <- lm(a ~ X1)$coef[1]  
}

plot(1:1000, beta0, type = "l", col = "black", ylim = c(-2.2, 1.6))
lines(1:1000, beta1, col = "red")
lines(1:1000, beta2, col = "blue")

model_lm <- lm(Y ~ X1 + X2)
abline(h = model_lm$coef[1], lty = "dashed")
abline(h = model_lm$coef[2], lty = "dashed")
abline(h = model_lm$coef[3], lty = "dashed")

p <- 100
n <- 1000
x <- matrix(ncol = p, nrow = n)
coefi <- rep(NA, p)
for(i in 1:p){
  x[,i] <- rnorm(n)
  coefi[i] <- rnorm(1) * 100
}
y <- x %*% coefi + rnorm(n)
beta <- rep(0,p)
max_iterations <- 1000
errors <- rep(NA, max_iterations + 1)
iter <- 2
errors[1] <- Inf
errors[2] <- sum(y - x %*% beta)^2
threshold <- 1e-04
while(iter < max_iterations && (errors[iter -1] - errors[iter]) > threshold){
  for(i in 1:p){
    a <- y - x %*% beta + beta[i] * x[,i]
    beta[i] <- lm(a ~ x[,i])$coef[2]
  }
  
  iter <- iter + 1
  errors[iter] <- sum((y - x %*% beta)^2)
  print(c(iter -2, errors[iter -1], errors[iter]))
}
plot(1:11, errors[3:13])
