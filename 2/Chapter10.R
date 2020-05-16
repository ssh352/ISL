states <- row.names(USArrests)
states
names(USArrests)
apply(USArrests, 2, mean)
apply(USArrests, 2, var)
pcr_out <- prcomp(USArrests, scale = T)
names(pcr_out)
pcr_out$center
pcr_out$scale
pcr_out$rotation
dim(pcr_out$x)
biplot(pcr_out, scale = 0)
pcr_out$rotation <- -pcr_out$rotation
pcr_out$x <- -pcr_out$x
biplot(pcr_out, scale = 0)
pcr_out$sdev
pcr_var <- pcr_out$sdev^2
pcr_var
pve <- pcr_var/sum(pcr_var)
pve
plot(pve)
plot(cumsum(pve))
a <- c(1, 2, 8, -3)
cumsum(a)

set.seed(2)
x <- matrix(rnorm(50 * 2), ncol = 2)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4
km <- kmeans(x, 2, nstart = 20)
km$cluster
plot(x, col = (km$cluster + 1))

km <- kmeans(x, 3, nstart = 20)
km
plot(x, col = (km$cluster + 1))

set.seed(3)
km <- kmeans(x, 3, nstart = 1)
km$tot.withinss
km <- kmeans(x, 3, nstart = 20)
km$tot.withinss

hc_complete <- hclust(dist(x), method = "complete")
hc_average <- hclust(dist(x), method = "average")
hc_single <- hclust(dist(x), method = "single")
plot(hc_complete)
plot(hc_average)
plot(hc_single)
cutree(hc_complete, 2)
cutree(hc_average,2)
cutree(hc_single,2)
cutree(hc_single, 4)

xsc <- scale(x, center = F, scale = T)
plot(hclust(dist(xsc), method = "complete"))

x <- matrix(rnorm(30 * 3), ncol = 3)
dd <- as.dist(1 - cor(t(x)))
plot(hclust(dd, method = "complete"))

library(ISLR)
nci_labs <- NCI60$labs
nci_data <- NCI60$data
dim(nci_data)
nci_labs

pcr_out <- prcomp(nci_data, scale = T)
Cols <- function(vec){
  cols <- rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}
plot(pcr_out$x[, 1:2], col = Cols(nci_labs))
plot(pcr_out$x[, c(1,3)], col = Cols(nci_labs))
summary(pcr_out)
plot(pcr_out)

pve <- 100 * pcr_out$sdev^2 / sum(pcr_out$sdev^2)
plot(pve)
plot(cumsum(pve))

sd_data <- scale(nci_data, F, T)
data_dist <- dist(sd_data)
plot(hclust(data_dist), labels = nci_labs)
plot(hclust(data_dist, method = "average"), labels = nci_labs)
plot(hclust(data_dist, method = "single"), labels = nci_labs)

hc_out <- hclust(dist(sd_data))
hc_clusters <- cutree(hc_out, 4)
table(hc_clusters, nci_labs)
plot(hc_out, labels = nci_labs)
abline(h = 139, col = "red")
hc_out
set.seed(2)
km.out <- kmeans(sd_data, 4, nstart = 20)
km.clusters <- km.out$cluster
table(km.clusters, hc_clusters)
hc_out <- hclust(dist(pcr_out$x[, 1:5]))
plot(hc_out)
table(cutree(hc_out,4), nci_labs)

set.seed(1)
x <- cbind(c(1, 1, 0, 5, 6, 4), c(4, 3, 4, 1, 2,0))
plot(x)
labels <- sample(2, nrow(x), replace = T)
center1 <- c(mean(x[labels==1, 1]), mean(x[labels==1, 2]))
center2 <- c(mean(x[labels==2, 1]), mean(x[labels==2, 2]))
center1
center2

plot(x[,1], x[,2], col = (labels + 1))
points(center1[1], center1[2], col = 2, pch = 4)
points(center2[1], center2[2], col = 3, pch = 4)

distance <- function(a, b){
  return(sqrt((a[1] - b[1])^2 + (a[2] - b[2])^2))
}

assign_labels <- function(x, center1, center2){
  labels <- rep(NA, nrow(x))
  for(i in 1: nrow(x)){
    if(distance(x[i,], center1) < distance(x[i,], center2)){
      labels[i] <- 1
    }else{
      labels[i] <- 2
    }
  }
  
  return(labels)
}

labels <- assign_labels(x, center1, center2)
labels

last_labels <- rep(-1, nrow(x))
while(!all(last_labels == labels)){
  last_labels <- labels
  center1 <- c(mean(x[labels==1, 1]), mean(x[labels==1, 2]))
  center2 <- c(mean(x[labels==2, 1]), mean(x[labels==2, 2]))
  labels <- assign_labels(x, center1, center2)

}
labels
plot(x[,1], x[,2], col = (labels + 1))
points(center1[1], center1[2], col = 2, pch = 4)
points(center2[1], center2[2], col = 3, pch = 4)


set.seed(1)
Control <- matrix(rnorm(50*1000), ncol=50)
Treatment <- matrix(rnorm(50*1000), ncol=50)
X <- cbind(Control, Treatment)
X[1,] <- seq(-18, 18 - .36, .36) 
pr.out <- prcomp(scale(X))
summary(pr.out)$importance[,1]
X <- rbind(X, c(rep(10, 50), rep(0, 50)))
pr.out <- prcomp(scale(X))
summary(pr.out)$importance[,1]

library(ISLR)

scaled <- scale(USArrests)

a <- dist(scaled)^2
b <- as.dist(1 - cor(t(scaled)))
summary(a/b)

pcr_out <- prcomp(USArrests, center = T, scale = T)
pcr_var <- pcr_out$sdev ^ 2
pve <- pcr_var / sum(pcr_var)
pve

loadings <- pcr_out$rotation
pve2 <- rep(NA, 4)
dmean <- apply(USArrests, 2, mean)
dsdev <- sqrt(apply(USArrests, 2, var))
dsc <- sweep(USArrests, 2, dmean, "-")
dsc <- sweep(dsc, 2, dsdev, "/")

for (i in 1:4) {
  proto_x <- sweep(dsc, MARGIN=2, loadings[,i], "*")
  pc_x <- apply(proto_x, 1, sum)
  pve2[i] <- sum(pc_x^2)
}
pve2 <- pve2/sum(dsc^2)
pve2

hc_comp <- hclust(dist(USArrests), method = "complete")
plot(hc_comp)
cutree(hc_comp, 3)
table(cutree(hc_comp, 3))

scaled <- scale(USArrests)
hc_comp_scale <- hclust(dist(scaled), method = "complete")
plot(hc_comp_scale)

table(cutree(hc_comp, 3), cutree(hc_comp_scale, 3))

set.seed(2)
x <- matrix(rnorm(20*3*50, mean=0, sd=0.001), ncol=50)
x[1:20, 2] <- 1
x[21:40, 1] <- 2
x[21:40, 2] <- 2
x[41:60, 1] <- 1

pca <- prcomp(x)

plot(pca$x[,1:2])

km <- kmeans(x, 3, nstart = 20)
table(c(rep(1,20), rep(2,20), rep(3,20)), km$cluster)

km_2 <- kmeans(x, 2, nstart = 20)
km_2

km_4 <- kmeans(x, 4, nstart = 20)
km_4

km_pca <- kmeans(pca$x[,1:2], 3, nstart = 20)
table(c(rep(1,20), rep(2,20), rep(3,20)), km_pca$cluster)

km_scale <- kmeans(scale(x), 2, nstart = 20)
km_scale

data <- read.csv("Ch10Ex11.csv", header = F)
dim(data)
dd <- as.dist(1 - cor(data))
hc_comp <- hclust(dd, method = "complete")
plot(hc_comp)

hc_ave <- hclust(dd, method = "ave")
plot(hc_ave)

hc_single <- hclust(dd, method = "single")
plot(hc_single)
