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

