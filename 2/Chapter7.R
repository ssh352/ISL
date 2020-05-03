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
