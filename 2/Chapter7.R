library(ISLR)
attach(Wage)

model <- lm(wage ~ poly(age, 4), data = Wage)
coef(summary(model))

model2 <- lm(wage ~ poly(age, 4, raw = T), data = Wage)
coef(summary(model))

model2a <- lm(wage ~ age + I(age ^2) + I(age ^ 3) + I(age ^ 4), data= Wage)
coef(model2a)

model2b <- lm(wage ~ cbind(age, age^2, age^3, age^4), data = Wage)

agelines <- range(age)
age_grid <- seq(from = agelines[1], to = agelines[2])
preds <- predict(model, newdata = list(age = age_grid), se = T)
se_bands <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)
par(mfrow = c(1,2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
plot(age , wage, xlim = agelines, cex = 0.5, col = "darkgray")
lines(age_grid, preds$fit, lwd = 2, col = "blue")
matlines(age_grid, se_bands, lwd = 1, col = "blue", lty = 3)

preds2 <- predict(model2, newdata = list(age = age_grid), se = T)
max(abs(preds$fit - preds2$fit))

model_1 <- lm(wage ~ age, data = Wage)
model_2 <- lm(wage ~ poly(age, 2), data = Wage)
model_3 <- lm(wage ~ poly(age, 3), data = Wage)
model_4 <- lm(wage ~ poly(age, 4), data = Wage)
model_5 <- lm(wage ~ poly(age, 5), data = Wage)
anova(model_1, model_2, model_3, model_4, model_5)

coef(summary(model_5))

model_1 <- lm(wage ~ education + age, data = Wage)
model_2 <- lm(wage ~ education + poly(age, 2), data = Wage)
model_3 <- lm(wage ~ education + poly(age, 3), data = Wage)
anova(model_1, model_2, model_3)

model <- glm(I(wage > 250) ~ poly(age, 4), data = Wage, family = binomial)
preds <- predict(model, newdata = list(age = age_grid), se = T)
pfit <- exp(preds$fit) / (1 + exp(preds$fit))
se_bands_logit <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)
se_bands <- exp(se_bands_logit)/(1 + exp(se_bands_logit))
plot(age, I(wage > 250), xlim = agelines, type = "n", ylim = c(0, 1))

points(jitter(age, I(wage > 250)/5), cex = 0.5, pch = "l")
lines(age_grid, pfit, lwd = 2, col = "blue")
matlines(age_grid, se_bands, lwd = 1, col = "blue", lty = 3)
table(cut(age, 4))

model <- lm(wage ~ cut(age, 4), data = Wage)
coef(summary(model))

library(ISLR)
library(splines)
fit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)
pred <- predict(fit, newdata = list(age = age_grid), se = T)
plot(age, wage, col = "gray")
lines(age_grid, pred$fit, lwd = 2)
lines(age_grid, pred$fit + 2 * pred$se, lty = "dashed")
lines(age_grid, pred$fit - 2 * pred$se, lty = "dashed")

dim(bs(age, knots = c(25, 40, 60)))
dim(bs(age, df = 6))
attr(bs(age, df = 6), "knots")

fit2 <- lm(wage ~ ns(age, df = 4), data = Wage)
pred2 <- predict(fit2, newdata = list(age = age_grid), se = T)
lines(age_grid, pred2$fit, col = "red", lwd = 2)

plot(age, wage, xlim = agelines, cex = 0.5, col = "darkgrey")
fit <- smooth.spline(age, wage, df = 16)
fit2 <- smooth.spline(age, wage, cv = T)
fit2$df

lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)

plot(age, wage, xlim = agelines, cex = 0.5, col = "darkgray")
fit <- loess(wage ~ age, span = 0.2, data = Wage)
fit2 <- loess(wage ~ age, span = 0.5, data = Wage)
lines(age_grid, predict(fit, data.frame(age = age_grid)), col = "red", lwd = 2)
lines(age_grid, predict(fit2, data.frame(age = age_grid)), col = "blue", lwd = 2)

gam1 <- lm(wage ~ ns(year, 4) + ns(age, 5) + education, darta = Wage)

library(gam)
gam_m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)
par(mfrow  = c(1,3))
plot(gam_m3, se = T, col = "blue")
plot.gam(gam1, se = T, col = "red")

gam_ma1 <- gam(wage ~ s(age, 5) + education, data = Wage)
gam_ma2 <- gam(wage ~ year + s(age, 5) + education, data = Wage)
anova(gam_ma1, gam_ma2, gam_m3)
summary(gam_m3)

preds <- predict(gam_ma2, newdata = Wage)
gam_lo <- gam(wage ~ s(year, df = 4) + lo(age, span = 0.7) + education, data = Wage)
gam_lo_i <- gam(wage ~ lo(year, age, span = 0.5) + education, data = Wage)

library(akima)
plot(gam_lo_i)

gam_lr <- gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial, data = Wage)
par(mfrow = c(1, 3))
plot(gam_lr, se = T, col = "green")
table(education, I(wage > 250))

gam_lr_s <- gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial, 
                data = Wage, subset = (education!="1.0<HS Grad"))
plot(gam_lr_s, se = T, col = "green")


set.seed(1)
library(ISLR)
library(boot)
attach(Wage)

deltas <- rep(NA, 10)
for(i in 1:10){
  model <- glm(wage ~ poly(age, i), data = Wage)
  deltas[i] <- cv.glm(Wage, model, K = 10)$delta[2]
}
plot(1:10, deltas)

which.min(deltas)

model_1 <- lm(wage ~ age, data = Wage)
model_2 <- lm(wage ~ poly(age, 2), data = Wage)
model_3 <- lm(wage ~ poly(age, 3), data = Wage)
model_4 <- lm(wage ~ poly(age, 4), data = Wage)
model_5 <- lm(wage ~ poly(age, 5), data = Wage)
model_6 <- lm(wage ~ poly(age, 6), data = Wage)
model_7 <- lm(wage ~ poly(age, 7), data = Wage)
model_8 <- lm(wage ~ poly(age, 8), data = Wage)
model_9 <- lm(wage ~ poly(age, 9), data = Wage)
model_10 <- lm(wage ~ poly(age, 10), data = Wage)
anova(model_1, model_2, model_3, model_4, model_5,
      model_6, model_7, model_8, model_9, model_10)

plot(wage~age, data=Wage, col="darkgrey")
agelims <- range(Wage$age)
age_grid <- seq(from=agelims[1], to=agelims[2])
lm_fit <- lm(wage~poly(age, 3), data=Wage)
lm_pred <- predict(lm_fit, data.frame(age=age_grid))
lines(age_grid, lm_pred, col="blue", lwd=2)

cuts <- rep(NA, 9)
for(i in 2:10){
  Wage$age_cut <- cut(Wage$age,i)
  model_cut <- glm(wage ~ age_cut, data = Wage)
  cuts[i-1] <- cv.glm(Wage, model_cut, K = 10)$delta[2]
}
plot(2:10, cuts)
model <- glm(wage ~ cut(age,8), data = Wage)
pred <- predict(model, data.frame(age=age_grid))
plot(wage~age, data=Wage, col="darkgrey")
lines(age_grid, pred, col="blue", lwd=2)

plot(maritl, wage)
plot(jobclass, wage)

model <- lm(wage ~ maritl, data = Wage)
deviance(model)

model <- lm(wage ~ jobclass, data = Wage)
deviance(model)

model <- lm(wage ~ maritl + jobclass, data = Wage)
deviance(model)

library(gam)
model <- gam(wage ~ maritl + jobclass + s(age, 4), data = Wage)
deviance(model)

pairs(Auto)

rss = rep(NA, 10)
fits = list()
for(i in 1:10){
  fits[[i]] <- lm(mpg ~ poly(displacement,i), data = Auto)
  rss[i] = deviance(fits[[i]])
}
rss
plot(rss)
anova(fits[[1]], fits[[2]], fits[[3]], fits[[4]])

errors <- rep(NA, 15)
for(i in 1:15){
  model <- glm(mpg ~ poly(displacement, i), data = Auto)
  errors[i] <- cv.glm(Auto, model, K = 10)$delta[2]
}
plot(errors)

errors <- rep(NA, 9)
for(i in 2:10){
  Auto$dis_cut <- cut(Auto$displacement, i)
  model <- glm(mpg ~ dis_cut, data = Auto)
  errors[i-1] <- cv.glm(Auto, model, K = 10)$delta[2]
}
plot(errors)

library(splines)
errors <- rep(NA, 8)
for(i in 3:10){
  model <- glm(mpg ~ ns(displacement, i), data = Auto)
  errors[i-2] <- cv.glm(Auto, model, K = 10)$delta[2]
}
plot(3:10, errors)

model <- glm(mpg ~ s(displacement, 4) + s(horsepower, 4), data = Auto)
summary(model)

library(ISLR)
library(MASS)
set.seed(1)
attach(Boston)

model <- glm(nox ~ poly(dis, 3), data = Boston)
summary(model)

dislim <- range(dis)
dis_grid <- seq(from = dislim[1], to = dislim[2], by = 0.1)
pred <- predict(model, list(dis = dis_grid))
plot(nox ~ dis)
lines(dis_grid, pred, col = "red")

rss <- rep(NA, 10)
for(i in 1:10){
  model <- lm(nox ~ poly(dis, i), data = Boston)
  rss[i] <- sum(model$residuals^2)
}
plot(rss)

library(boot)

rss <- rep(NA, 10)
for(i in 1:10){
  model <- glm(nox ~ poly(dis, i), data = Boston)
  rss[i] <- cv.glm(Boston, model, K = 10)$delta[2]
}
plot(rss)
library(splines)

model_bs <- lm(nox ~ bs(dis, df = 4, knots = c(4, 7, 11)), data = Boston)
summary(model_bs)

pred_bs <- predict(model_bs, list(dis = dis_grid))
plot(nox, dis, col = "darkgray")
lines(dis_grid, pred_bs, col = "red")

cv <- rep(NA, 14)
for(i in 3:16){
  model <- lm(nox ~ bs(dis, df = i), data = Boston)
  cv[i-2] <- sum(model$residuals^2)
}
plot(3:16, cv)

cv <- rep(NA, 14)
for(i in 3:16){
  model <- glm(nox ~ bs(dis, df = i), data = Boston)
  cv[i-2] <- cv.glm(Boston, model, K = 10)$delta[2]
}
plot(3:16, cv)

library(leaps)
attach(College)
train <- sample(length(Outstate), length(Outstate)/2)
test <- -train
train_data <- College[train,]
test_data <- College[test,]

model_fwd <- regsubsets(Outstate ~ ., data = test_data, nvmax = 17, method = "forward")
summary(model_fwd)
plot(summary(model_fwd)$cp)
which.min(summary(model_fwd)$cp)
coefi <- coef(model_fwd, id = 6)
names(coefi)

library(gam)
gam.fit <- gam(Outstate ~ Private + s(Room.Board, df = 2) + s(PhD, df = 2) + 
                s(perc.alumni, df = 2) + s(Expend, df = 5) + s(Grad.Rate, df = 2), data = train_data)
par(mfrow = c(2, 3))
plot(gam.fit, se = T, col = "blue")
pred <- predict(gam.fit, test_data)
mean((test_data$Outstate - pred)^2)
gam.tss <- mean((test_data$Outstate - mean(test_data$Outstate))^2)
 1 - mean((test_data$Outstate - pred)^2)/gam.tss

summary(gam.fit)
 
set.seed(1)
x1 <- rnorm(100)
x2 <- rnorm(100)
eps <- rnorm(100, sd = 0.1)
y <- -2.1 + 1.3 * x1 + 0.54 * x2 + eps

beta0 <- rep(NA, 1000)
beta1 <- rep(NA, 1000)
beta2 <- rep(NA, 1000)
beta1[1] <- 10

for(i in 1:1000){
  a <- y - beta1[i] * x1
  beta2[i] <- lm(a ~ x2)$coef[2]
  a <- y - beta2[i] * x1
  model <- lm(a ~ x1)
  if(i < 1000){
    beta1[i + 1] <- model$coef[2]
  }
  beta0[i] <- model$coef[1]
}
plot(1:1000, beta0, col = "black",ylim = c(-2.2, 1.6),)
lines(1:1000, beta1, col = "red")
lines(1:1000, beta2, col = "green")

model <- lm(y ~ x1 + x2)
abline(h = model$coef[1])
abline(h = model$coef[2])
abline(h = model$coef[3])

set.seed(1)
p <- 100
n <- 1000
x <- matrix(ncol = p, nrow = n)
coefi <- rep(NA, p)
for (i in 1:p) {
  x[, i] <- rnorm(n)
  coefi[i] <- rnorm(1) * 100
}
y <- x %*% coefi + rnorm(n)

beta <- rep(0, p)
max_iterations <- 1000
errors <- rep(NA, max_iterations + 1)
iter <- 2
errors[1] <- Inf
errors[2] <- sum((y - x %*% beta)^2)
threshold <- 1e-04
while (iter < max_iterations && errors[iter - 1] - errors[iter] > threshold) {
  for (i in 1:p) {
    a <- y - x %*% beta + beta[i] * x[, i]
    beta[i] <- lm(a ~ x[, i])$coef[2]
  }
  iter <- iter + 1
  errors[iter] <- sum((y - x %*% beta)^2)
  print(c(iter - 2, errors[iter - 1], errors[iter]))
}
plot(1:11, errors[3:13])
