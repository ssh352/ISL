x <- c(1, 3, 2, 5)
x

x = c(1, 6, 2)
x
y = c(1, 4, 3)

length(x)
length(y)
x + y

ls()
rm(x , y)
ls()
rm(list = ls())

?matrix

x = matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2)
x
matrix(data = c(1, 2, 3, 4), 2, 2, byrow = TRUE)

sqrt(x)
x ^ 2

x = rnorm(50)
y = x + rnorm(50, mean = 50, sd = .1)
cor(x, y)

set.seed(1303)
rnorm(50)

set.seed(3)
y = rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)

x = rnorm(100)
y = rnorm(100)
plot(x, y)
plot(x, y, xlab = "x軸", ylab = "y軸", main = "XとYのプロット")
pdf("Figure.pdf")
plot(x, y, col = "green")
dev.off()

x = seq(1, 10)
x
x = 1:10
x
x = seq(-pi, pi, length = 50)

y = x
f = outer(x, y, function(x, y)cos(y)/(1 + x^2))
contour(x, y, f)
contour(x, y, f, nlevels = 45, add = T)
fa = (f - t(f))/2
contour(x, y, fa, nlevels = 15)

image(x, y, fa)
persp(x, y, fa)
persp(x, y, fa, theta = 30)
persp(x, y, fa, theta = 30, phi = 20)
persp(x, y, fa, theta = 30, phi = 70)
persp(x, y, fa, theta = 30, phi = 40)

A = matrix(1:16, 4, 4)
A
A[2,3]
A[c(1,3), c(2,4)]
A[1:3, 2:4]
A[1:2,]
A[,1:2]
A[1,]
A[-c(1,3),]
dim(A)

Auto = read.table("Auto.data")
fix(Auto)

Auto = read.csv("Auto.csv", header = T, na.string = "?")
Auto
Auto = na.omit(Auto)
dim(Auto)
names(Auto)
plot(Auto$cylinders, Auto$mpg)
attach(Auto)
plot(cylinders, mpg)
cylinders = as.factor(cylinders)
plot(cylinders,mpg)
plot(cylinders,mpg, col = "red")
plot(cylinders,mpg, col = "red", varwidth = "T")
plot(cylinders,mpg, col = "red", varwidth = "T", horizontal = T)
plot(cylinders,mpg, col = "red", varwidth = "T", xlab = cylinders, ylab = "MPG")

hist(mpg)
hist(mpg, col = 2)
hist(mpg, col = 2, breaks = 15)
pairs(Auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)

plot(horsepower, mpg)
identify(horsepower, mpg, name)

summary(Auto)

summary(mpg)

curve(82*x, from=0, to=10, xlab="flexibility", ylab="MSE", col="white")  
curve(300*cos(x/3)+500+x^3/3, add=TRUE, col="red", lwd=2)  
curve(x^3/3, add=TRUE, col="orange", lwd=2)                
curve(0*x+250, add=TRUE, col="gray", lwd=2)                
curve(300*cos(x/3)+350, add=TRUE, col="green", lwd=2)      
curve(225*cos(x/3)+450, add=TRUE, col="blue", lwd=2)       

data <- matrix(0, nrow = 6, ncol = 3)
data[1,] <- c(0, 3, 0)
data[2,] <- c(2, 0, 0)
data[3,] <- c(0, 1, 3)
data[4,] <- c(0, 1, 2)
data[5,] <- c(-1, 0, 1)
data[6,] <- c(1, 0, 2)
Y <- c(0, 0, 0, 1, 1, 0)
X <- c(0, 0, 0)

dist <- c(0, 0, 0, 0, 0, 0)
dist[1] <- sqrt(sum((data[1,] - X)^2))
dist[2] <- sqrt(sum((data[2,] - X)^2))
dist[3] <- sqrt(sum((data[3,] - X)^2))
dist[4] <- sqrt(sum((data[4,] - X)^2))
dist[5] <- sqrt(sum((data[5,] - X)^2))
dist[6] <- sqrt(sum((data[6,] - X)^2))
dist

college <- read.csv("College.csv")
rownames(college) = college[,1]
college <- college[,-1]
summary(college)
pairs(college[,1:10])
attach(college)
Private = as.factor(Private)
plot(Private, Outstate)

Elite <- rep("No", nrow(college))
Elite[Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
college = data.frame(college, Elite)
summary(college)
plot(Elite, Outstate)

par(mfrow=c(2,2))
hist(Apps, breaks=50, xlim=c(0,25000), main="Apps")
hist(Enroll, breaks=25, main="Enroll")
hist(Expend, breaks=25, main="Expend")
hist(Outstate, main="Outstate")

require(ISLR)
data(Auto)
Auto <- na.omit(Auto)

range(Auto$mpg)
range(Auto$cylinders)
range(Auto$displacement)
range(Auto$horsepower)
range(Auto$weight)
range(Auto$acceleration)
range(Auto$year)

sapply(Auto[,1:7], mean)
sapply(Auto[,1:7], sd)


tmp <- Auto[,-(8:9)]
tmp <- tmp[-(10:85),]
sapply(tmp, range)
sapply(tmp, mean)
sapply(tmp, sd)

pairs(Auto[,1:7])

install.packages("MASS")
library(MASS)
print(Boston)
data(Boston)
pairs(Boston)
