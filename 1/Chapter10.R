states <- row.names(USArrests)
states
names(USArrests)

apply(USArrests, 2, mean)
apply(USArrests, 2, var)

pca <- prcomp(USArrests, scale = TRUE)
names(pca)
pca$center
pca$scale
pca$rotation
dim(pca$x)

biplot(pca, scale = 0)

pca$rotation <- -pca$rotation
pca$x <- -pca$x
biplot(pca, scale = 0)

pca$sdev

pca_var <- pca$sdev ^ 2
pca_var

pve <- pca_var / sum(pca_var)
plot(pve)
plot(cumsum(pve))

a <- c(1, 2, 8, -3)
cumsum(a)

set.seed(2)
x <- matrix(rnorm(50 * 2), ncol = 2)
x[1:25, 1] = x[1:25, 1] + 3
x[1:25, 2] = x[1:25, 2] - 4
plot(x)
model_kmeans <- kmeans(x, 2, nstart = 20)
model_kmeans$cluster

plot(x, col = model_kmeans$cluster + 1)

set.seed(4)
model_kmeans_3 <- kmeans(x, 3, nstart = 20)
model_kmeans_3
plot(x, col = model_kmeans_3$cluster + 1)

set.seed(3)
model_km <- kmeans(x, 3, nstart = 1)
model_km$tot.withinss
model_km <- kmeans(x, 3, nstart = 20)
model_km$tot.withinss

model_comp <- hclust(dist(x), method = "complete")
model_ave <- hclust(dist(x), method = "ave")
model_single <- hclust(dist(x), method = "single")

par(mfrow = c(1,3))
plot(model_comp)
plot(model_ave)
plot(model_single)

cutree(model_comp, 2)
cutree(model_ave, 2)
cutree(model_single, 2)
cutree(model_single, 4)

xsc <- scale(x, center = T, scale = T)
plot(hclust(dist(xsc), method = "complete"))

x <- matrix(rnorm(30 * 3), ncol = 3)
dd <- as.dist(1 - cor(t(x)))
plot(hclust(dd, method = "complete"))

library(ISLR)
labs <- NCI60$labs
data <- NCI60$data

dim(data)
labs[1:4]
table(labs)

pca <- prcomp(data, scale = T)
Cols <- function(vec){
  cols <- rainbow(length(unique(vec)))
  return (cols[as.numeric(as.factor(vec))])
}
par(mfrow = c(1,2))
plot(pca$x[,1:2], col = Cols(labs))
plot(pca$x[,c(1,3)], col = Cols(labs))
summary(pca)
plot(pca)
pve <- 100 * pca$sdev ^ 2 / sum(pca$sdev ^ 2)
par(mfrow = c(1,2))
plot(pve)
plot(cumsum(pve))

sd_data <- scale(data, F, T)
par(mfrow = c(1,1))
data.dist <- dist(sd_data)
plot(hclust(data.dist))
plot(hclust(data.dist, method = "average"))
plot(hclust(data.dist, method = "single"))
hc_out <- hclust(dist(sd_data))
hc_clusters <- cutree(hc_out, 4)
table(hc_clusters, labs)

par(mfrow = c(1,1))
plot(hc_out)
abline(h = 139, col = "red")
hc_out

set.seed(2)
km_out <- kmeans(sd_data, 4, nstart = 20)
km_clusters <- km_out$cluster
table(km_clusters, hc_clusters)

hc_out <- hclust(dist(pca$x[,1:5]))
plot(hc_out)

x <- cbind(c(1, 1, 0, 5, 6, 4), c(4, 3, 4, 1, 2, 0))
x
plot(x[,1], x[,2])
lavels <- sample(2, nrow(x), replace=T)
lavels

center1 <- c(mean(x[lavels == 1, 1]), mean(x[lavels == 1, 2]))
center2 <- c(mean(x[lavels == 2, 1]), mean(x[lavels == 2, 2]))
center1
center2
plot(x[,1], x[,2], col=(lavels+1))
points(center1[1], center1[2], col = 2)
points(center2[1], center2[2], col = 3)

euclid = function(a, b) {
  return(sqrt((a[1] - b[1])^2 + (a[2]-b[2])^2))
}
assign_labels = function(x, center1, center2) {
  labels = rep(NA, nrow(x))
  for (i in 1:nrow(x)) {
    if (euclid(x[i,], center1) < euclid(x[i,], center2)) {
      labels[i] = 1
    } else {
      labels[i] = 2
    }
  }
  return(labels)
}
labels = assign_labels(x, center1, center2)
labels

last_labels = rep(-1, 6)
while (!all(last_labels == labels)) {
  last_labels = labels
  center1 = c(mean(x[labels==1, 1]), mean(x[labels==1, 2]))
  center2 = c(mean(x[labels==2, 1]), mean(x[labels==2, 2]))
  print(center1)
  print(center2)
  labels = assign_labels(x, center1, center2)
}

plot(x[,1], x[,2], col=(lavels+1))
points(center1[1], center1[2], col = 2)
points(center2[1], center2[2], col = 3)

library(ISLR)

scaled <- scale(USArrests)
a <- dist(scaled) ^ 2
b <- as.dist(1 - cor(t(scaled)))
summary(b / a)

set.seed(1)
pca <- prcomp(USArrests, center = T, scale = T)
pca_var <- pca$sdev ^ 2
pve <- pca_var / sum(pca_var)
pve

rotation <- pca$rotation
pve2 <- rep(NA, 4)
dmean <- apply(USArrests, 2, mean)
dsdev <- apply(USArrests, 2, mean)
dsc <- sweep(USArrests, MARGIN = 2, dmean, "-")
dsc <- sweep(dsc, MARGIN = 2, dmean, "/")
for( i in 1:4){
  proto_X <- sweep(dsc, MARGIN = 2, rotation[i], "*")
  pc_X <- apply(proto_X, 1, sum)
  pve2[i] <- sum(pc_X ^ 2)
}
pve2 <- pve2 / sum(dsc ^ 2)
pve2

set.seed(2)
model_comp <- hclust(dist(USArrests), method = "complete")
plot(model_comp)

cutree(model_comp, 3)
table(cutree(model_comp, 3))

scaled <- scale(USArrests)
model_scaled_comp <- hclust(dist(USArrests), method = "complete")
plot(model_scaled_comp)

cutree(model_scaled_comp, 3)
table(cutree(model_scaled_comp, 3))

set.seed(2)
x <- matrix(rnorm(20 * 3 * 50, mean = 0, sd = 0.001), ncol = 50)
x[1:20, 2] <- 1
x[21:40, 1] <- 2
x[21: 40, 2] <- 2
x[41: 60, 1] <- 1

pca <- prcomp(x)
summary(pca)
pca$x[, 1:2]

plot(pca$x[, 1:2], col = 2:4)

km <- kmeans(x, 3, nstart = 20)
table(km$cluster, c(rep(1, 20), rep(2, 20), rep(3,20)))

km2 <- kmeans(x, 2, nstart = 20)
km2$cluster

km_pca <- kmeans(pca$x[, 1:2], 3, nstart = 20)
table(km_pca$cluster, c(rep(1, 20), rep(2, 20), rep(3,20)))

km_scaled <- kmeans(scale(x), 3, nstart = 20)
km_scaled$cluster

data <- read.csv("./ch10Ex11.csv", header = F)
dim(data)

dd <- as.dist(1 - cor(data))
plot(hclust(dd, method = "complete"))
plot(hclust(dd, method = "single"))
plot(hclust(dd, method = "average"))

pca <- prcomp(t(data))
summary(pca)
total_load <- apply(pca $ rotation, 1, sum)
indices <- order(abs(total_load), decreasing = T)
indices[1:10]
total_load[indices[1:10]]
