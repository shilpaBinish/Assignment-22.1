getwd()
setwd("D:\\R")
d<-read.csv("cluster.csv",na.strings = c(""," ",NA))
d
rdacb.scale.many <- function (d, column_nos) {
  nms <- names(d)
  for (col in column_nos) {
    name <- paste0(nms[col], "_z")
    d[name] <- scale(d[, col])
  }
  cat(paste("Scaled", length(column_nos), "variable(s)\n"))
  d
}
kmeans(d,centers = 4,nstart = 20)
plot(rating ~calories,d)
x<-d[,-c(1,1)]
x
m<-apply(x,2,mean)
s<-apply(x,2,sd)
z<-scale(x,m,s)
distance<-dist(z)
distance
print(distance,digit=3)
set.seed(20)
Cluster <- kmeans(g[, 3:4], 3, nstart = 20)
Cluster
table(Cluster$cluster, d$rating)
Cluster$cluster <- as.factor(Cluster$cluster)
ggplot(d, aes(rating, calorie, color = d$cluster)) + geom_point()