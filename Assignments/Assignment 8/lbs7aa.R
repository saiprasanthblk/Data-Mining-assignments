# k means

set.seed(2)
x=matrix (rnorm(50*2) , ncol =2)
x[1:25 ,1]=x[1:25 ,1]+3
x[1:25 ,2]=x[1:25 ,2] -4

km.out =kmeans (x,2, nstart =20)

km.out

plot(x, col =(km.out$cluster +1) , main="K-Means Clustering Results with K=2", xlab ="", ylab="", pch =20, cex =2)

set.seed(4)
km.out = kmeans(x,3, nstart =20)
km.out

set.seed(3)
km.out =kmeans(x,3, nstart =1)
km.out$tot.withinss
km.out =kmeans(x,3, nstart =20)
km.out$tot.withinss


hc.complete =hclust(dist(x), method ="complete")

hc.average =hclust(dist(x), method ="average")
hc.single =hclust(dist(x), method ="single")

par(mfrow =c(1,3))
plot(hc.complete ,main ="Complete Linkage", xlab="", sub ="", cex =.9)
plot(hc.average , main ="Average Linkage", xlab="", sub ="",cex =.9)
plot(hc.single , main="Single Linkage", xlab="", sub ="",cex =.9)

cutree(hc.complete, 2)
cutree(hc.average , 2)
cutree(hc.single , 2)

cutree(hc.single , 4)

xsc=scale(x)
plot(hclust(dist(xsc), method ="complete"), main =" Hierarchical Clustering with Scaled Features ")

x=matrix(rnorm(30*3) , ncol =3)
dd=as.dist(1- cor(t(x)))
plot(hclust (dd, method ="complete"), main="Complete Linkage with Correlation -Based Distance", xlab="", sub ="")


library(ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data

pr.out = prcomp(nci.data , scale=TRUE)

Cols=function(vec){
  cols=rainbow (length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
  }


par(mfrow =c(1,2))
plot(pr.out$x [,1:2], col =Cols(nci.labs), pch =19, xlab ="Z1", ylab="Z2")
plot(pr.out$x[,c(1,3)], col =Cols(nci.labs), pch =19, xlab ="Z1", ylab="Z3")

summary(pr.out)


pve =100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow = c(1,2))
plot(pve , type ="o", ylab="PVE", xlab=" Principal Component ", col ="blue")
plot(cumsum(pve), type="o", ylab ="Cumulative PVE", xlab= "Principal Component", col ="brown3")


sd.data=scale(nci.data)


par(mfrow =c(1,3))
data.dist=dist(sd.data)
plot(hclust (data.dist), labels =nci.labs , main=" Complete Linkage ", xlab ="", sub ="", ylab ="")
plot(hclust (data.dist , method =" average "), labels =nci.labs , main=" Average Linkage ", xlab ="", sub ="", ylab ="")
plot(hclust (data.dist , method =" single "), labels =nci.labs,main=" Single Linkage ", xlab="", sub ="", ylab ="")



hc.out =hclust(dist(sd.data))
hc.clusters =cutree(hc.out,4)
table(hc.clusters ,nci.labs)


par(mfrow =c(1,1))
plot(hc.out , labels =nci.labs)
abline(h=139, col ="red")


# exercises

# problem 2

# complete linkage

d = as.dist(matrix(c(0, 0.3, 0.4, 0.7, 
                     0.3, 0, 0.5, 0.8,
                     0.4, 0.5, 0.0, 0.45,
                     0.7, 0.8, 0.45, 0.0), nrow = 4))
plot(hclust(d, method = "complete"))

# simple linkage

plot(hclust(d, method = "single"))

# c: In this case, we have clusters (1,2) and (3,4).

# d: In this case, we have clusters ((1,2),3) and (4).

# part e

plot(hclust(d, method = "complete"), labels = c(2,1,4,3))

# problem 3

# part a

x <- cbind(c(1, 1, 0, 5, 6, 4), c(4, 3, 4, 1, 2, 0))
plot(x[,1], x[,2])

# part b

set.seed(1)
labels <- sample(2, nrow(x), replace = T)
labels

plot(x[, 1], x[, 2], col = (labels + 1), pch = 20, cex = 2)

# part c

centroid1 <- c(mean(x[labels == 1, 1]), mean(x[labels == 1, 2]))
centroid2 <- c(mean(x[labels == 2, 1]), mean(x[labels == 2, 2]))
plot(x[,1], x[,2], col=(labels + 1), pch = 20, cex = 2)
points(centroid1[1], centroid1[2], col = 2, pch = 4)
points(centroid2[1], centroid2[2], col = 3, pch = 4)

# part d

labels <- c(1, 1, 1, 2, 2, 2)
plot(x[, 1], x[, 2], col = (labels + 1), pch = 20, cex = 2)
points(centroid1[1], centroid1[2], col = 2, pch = 4)
points(centroid2[1], centroid2[2], col = 3, pch = 4)

# part e

centroid1 <- c(mean(x[labels == 1, 1]), mean(x[labels == 1, 2]))
centroid2 <- c(mean(x[labels == 2, 1]), mean(x[labels == 2, 2]))
plot(x[,1], x[,2], col=(labels + 1), pch = 20, cex = 2)
points(centroid1[1], centroid1[2], col = 2, pch = 4)
points(centroid2[1], centroid2[2], col = 3, pch = 4)

# plot f

plot(x[, 1], x[, 2], col=(labels + 1), pch = 20, cex = 2)


# problem 4

# a: the complete linkage height is always going to be atleast as much as the single linkage height. But they they could tell. Hence, we cannot say one will be larger

# part b: # definitely the same height

# problem 5

socks <- c(8, 11, 7, 6, 5, 6, 7, 8)
computers <- c(0, 0, 0, 0, 1, 1, 1, 1)
x <- cbind(socks, computers)
labels <- c(1, 1, 2, 2, 2, 2, 1, 1)
plot(x[, 1], x[, 2], col=(labels + 1), pch = 20, cex = 2, asp = 1)

x <- cbind(scale(socks, center = FALSE), scale(computers, center = FALSE))
sd(computers)

labels <- c(1, 1, 2, 2, 2, 2, 1, 1)
plot(x[, 1], x[, 2], col=(labels + 1), pch = 20, cex = 2, asp = 1)

# If we take into consideration the variables measured by the number of dollars spent, here also the number of computers plays a much larger role than the number of socks, so we have the clusters {5,6,7,8}{5,6,7,8} (purchased computer) and {1,2,3,4}{1,2,3,4} (no computer purchased).


# problem 7

library(ISLR)
set.seed(1)
dsc <- scale(USArrests)
d1 <- dist(dsc)^2
d2 <- as.dist(1 - cor(t(dsc)))
summary(d2 / d1)


# problem 9

# part a

set.seed(2)
hc.complete <- hclust(dist(USArrests), method = "complete")
plot(hc.complete)

# part b

cutree(hc.complete, 3)

# part c

sd.data <- scale(USArrests)
hc.complete.sd <- hclust(dist(sd.data), method = "complete")
plot(hc.complete.sd)

# part d

cutree(hc.complete.sd, 3)

table(cutree(hc.complete, 3), cutree(hc.complete.sd, 3))


# part 11

# part a

genes <- read.csv("Ch10Ex11.csv", header = FALSE)

# part b

hc.complete <- hclust(as.dist(1 - cor(genes)), method = "complete")
plot(hc.complete)

hc.single <- hclust(as.dist(1 - cor(genes)), method = "single")
plot(hc.single)

hc.average <- hclust(as.dist(1 - cor(genes)), method = "average")
plot(hc.average)

# part c

pr.out <- prcomp(t(genes))
head(pr.out$rotation)

total.load <- apply(pr.out$rotation, 1, sum)
index <- order(abs(total.load), decreasing = TRUE)
index[1:10]



















































































