library(swirl)
swirl()
dist(dataFrame)
min(dist(dataFrame))

hc <- hclust(distxy)
plot(hc)
plot(as.dendrogram(hc))
abline(h = 1.5, col = "blue")
abline(h = 0.4, col = "red")
abline(h = 0.05, col = "green" )
min(dist(dFsm))
dist(dFsm)
hc

heatmap(dataMatrix, col = cm.colors(25))
dataMatrix
heatmap(mt)
mt
plot(denmt)
distmt

cmat
points(cx, cy, col = c("red", "orange", "purple"), pch = 3, cex = 2, lwd = 2)
mdist(x,y,cx,cy)
apply(distTmp, 2, which.min)
points(x, y, pch = 19, cex = 2, col = cols1[newClust])

tapply(x, newClust, mean)
tapply(y, newClust, mean)
points(newCx, newCy, col = cols1, pch = 8, cex = 2, lwd = 2)
mdist(x, y, newCx, newCy)
apply(distTmp2, 2, which.min)
points(x, y, pch = 19, cex = 2, col = cols1[newClust2])

tapply(x, newClust2, mean)
tapply(y, newClust2, mean)
points(finalCx,finalCy,col=cols1,pch=9,cex=2,lwd=2)

kmeans(dataFrame, centers = 3)
kmObj$iter
plot(x, y, col = kmObj$cluster, pch = 19, cex = 2)
points(kmObj$centers,col=c("black","red","green"),pch=3,cex=3,lwd=3)

kmeans(dataFrame, centers = 6)
plot(x,y,col=kmeans(dataFrame,6)$cluster,pch=19,cex=2)



head(dataMatrix)
heatmap(dataMatrix)
myedit("addPatt.R")
source("addPatt.R", local=TRUE)
heatmap(dataMatrix)

mat
svd(mat)

matu %*% diag %*% t(matv)
svd(scale(mat))
prcomp(scale(mat))

svd1$v[,1]
svd1$d

head(constantMatrix)
svd2$d
svd2$v[,1:2]
svd2$d

dim(faceData)
a1 <- (svd1$u[,1] * svd1$d[1]) %*% t(svd1$v[,1])
a1 <- svd1$u[,1] %*% t(svd1$v[,1]) * svd1$d[1]
## only * is used for svd$d[1] because it is a single, constant value
myImage(a1)
a2 <- svd1$u[,1:2] %*% diag(svd1$d[1:2]) %*% t(svd1$v[,1:2]) 

##| Create the matrix a2 as the product of the first 2 columns of svd1$u, a diagonal matrix using the first 2 elements of
##| svd1$d, and the transpose of the first 2 columns of svd1$v. Since all of your multiplicands are matrices you have to
##| use only the operator %*% AND you DON'T need parentheses. Also, you must use the R function diag with svd1$d[1:2] as
##| its sole argument to create the proper diagonal matrix. Remember, matrix multiplication is NOT commutative so you have
##| to put the multiplicands in the correct order. Please use the 1:2 notation and not the c(m:n), i.e., the concatenate
##| function, when specifying the columns.
myImage(a2)
a5 <- svd1$u[,1:5] %*% diag(svd1$d[1:5]) %*% t(svd1$v[,1:5])
myImage(a5)
myImage(svd1$u[,1:10] %*% diag(svd1$d[1:10]) %*% t(svd1$v[,1:10]))

##| We'll close now with a few comments. First, when reducing dimensions you have to pay attention to the scales on which
##| different variables are measured and make sure that all your data is in consistent units. In other words, scales of
##| your data matter. Second, principal components and singular values may mix real patterns, as we saw in our simple
##| 2-pattern example, so finding and separating out the real patterns require some detective work. Let's do a quick
##| review now.



dim(ssd)

















































