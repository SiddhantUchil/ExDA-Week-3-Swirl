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
names(ssd)[562:563]
names(ssd[562:563])
table(ssd$subject)
sum(table(ssd$subject))
table(ssd$activity)
sum(table(ssd$activity))

sub1 <- subset(ssd, subject == 1)
dim(sub1)
names(sub1[, 1:12])

myedit("showXY.R")
par(mfrow=c(1, 2), mar = c(5, 4, 1, 1))
plot(sub1[, 1], col = sub1$activity, ylab = names(sub1)[1])
plot(sub1[, 2], col = sub1$activity, ylab = names(sub1)[2])
legend("bottomright",legend=unique(sub1$activity),col=unique(sub1$activity), pch = 1)
par(mfrow=c(1,1))
##the code of showXY.R


showMe(1:6)

library(fields)

# For compatibility with 2.2.21
.get_course_path <- function(){
  tryCatch(swirl:::swirl_courses_dir(),
           error = function(c) {file.path(find.package("swirl"),"Courses")}
  )
}

# Put initialization code in this file.
path_to_course <- file.path(.get_course_path(),
                            "Exploratory_Data_Analysis","Clustering_Example")
try(dev.off(),silent=TRUE)
plot.new()

pathtofile <- function(fileName){
  mypath <- file.path(.get_course_path(),
                      "Exploratory_Data_Analysis","Clustering_Example",
                      fileName)
}
fxfer <- function(fileName){
  mypath <- pathtofile(fileName)
  file.copy(mypath,fileName)
}

myImage <- function(iname){
  par(mfrow=c(1,1))
  par(mar=c(8,10,8,10))
  image(t(iname)[,nrow(iname):1])
}
myedit <- function(fname){
  #fxfer(fname)
  #file.edit(fname)
  mypath <- pathtofile(fname)
  file.edit(mypath)
}

mdist <- function(x,y,cx,cy){
  distTmp <- matrix(NA,nrow=3,ncol=12)
  distTmp[1,] <- (x-cx[1])^2 + (y-cy[1])^2
  distTmp[2,] <- (x-cx[2])^2 + (y-cy[2])^2
  distTmp[3,] <- (x-cx[3])^2 + (y-cy[3])^2  
  return(distTmp)
}

showMe <- function(cv){
  myarg <- deparse(substitute(cv))
  z<- outer( 1:20,1:20, "+")
  obj<- list( x=1:20,y=1:20,z=z )
  image(obj, col=cv, main=myarg  )
}
load(pathtofile("samsungData.rda"))
set.seed(1234);
par(mfrow=c(1, 2), mar = c(5, 4, 1, 1))
ssd <- transform(samsungData, activity = factor(activity))
source(pathtofile("myplclust.R"),local=TRUE)

##the above are the codes used in the global functions

mdist <- dist(sub1[, 1:3])
hclustering <- hclust(mdist)

myplclust <- function( hclust, lab=hclust$labels, lab.col=rep(1,length(hclust$labels)), hang=0.1,...){
  ## modifiction of plclust for plotting hclust objects *in colour*!
  ## Copyright Eva KF Chan 2009
  ## Arguments:
  ##    hclust:    hclust object
  ##    lab:        a character vector of labels of the leaves of the tree
  ##    lab.col:    colour for the labels; NA=default device foreground colour
  ##    hang:     as in hclust & plclust
  ## Side effect:
  ##    A display of hierarchical cluster with coloured leaf labels.
  y <- rep(hclust$height,2)
  x <- as.numeric(hclust$merge)
  y <- y[which(x<0)]
  x <- x[which(x<0)]
  x <- abs(x)
  y <- y[order(x)]
  x <- x[order(x)]
  plot( hclust, labels=FALSE, hang=hang, ... )
  text( x=x, y=y[hclust$order]-(max(hclust$height)*hang), labels=lab[hclust$order], col=lab.col[hclust$order], srt=90, adj=c(1,0.5), xpd=NA, ... )}

myplclust(hclustering, lab.col = unclass(sub1$activity))

mdist <- dist(sub1[,10:12])
hclustering <- hclust(mdist)
myplclust(hclustering, lab.col = unclass(sub1$activity))

svd1 <- svd(scale(sub1[,-c(562,563)]))

?scale()
##scale performs centering on each column.
##centering means subtracting the mean value of the column from every element in the column

dim(svd1$u)


maxCon <- which.max(svd1$v[,2])
mdist <- dist(c(sub1[, 10:12], maxCon))
mdist <- dist(sub1[,c(10:12,maxCon)])
hclustering <- hclust(mdist)
myplclust(hclustering, lab.col = unclass(sub1$activity))
names(sub1[maxCon])

kClust <- kmeans(sub1[, -c(562, 562)], centers = 6)
kClust <- kmeans(sub1[, -c(562, 563)], centers = 6)
table(kClust$cluster, sub1$activity)
kClust <- kmeans(sub1[, -c(562, 562)], centers = 6, nstart = 100)
kClust <- kmeans(sub1[, -c(562, 563)], centers = 6, nstart = 100)
table(kClust$cluster, sub1$activity)

dim(kClust$centers)
laying <- which(kClust$size == 29)
plot(kClust$centers[laying,1:12], pch = 19, ylab = "Laying Cluster")
names(sub1[1:3])

walkdown <- which(kClust$size==49)
plot(kClust$centers[walkdown, 1:12],pch=19,ylab="Walkdown Cluster")




































