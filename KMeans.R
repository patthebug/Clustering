library(maps)
library(ggmap)
library(ggplot2)
library(HDclassif)
library(pamr)
set.seed(99)
data(world.cities)
world.cities <- world.cities[which(world.cities$pop>1000000),]
citiesToPlot<-world.cities

# data.cl <- pam(world.cities, clusters, metric = 'manhattan')
# world.cities$clusters <- data.cl$clustering

R<-6371.0
world.cities$x <- R * cos(world.cities$lat) * cos(world.cities$long)
world.cities$y <- R * cos(world.cities$lat) * sin(world.cities$long)
world.cities$z <- R *sin(world.cities$lat)

# world.cities<-world.cities[,4:6]
# world.cities<-world.cities[,7:9]
data.pl <- world.cities

p <- ggplot(legend=FALSE) +
  geom_polygon( data=world, aes(x=long, y=lat,group=group)) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_blank(),axis.text.y = element_blank()) +
  theme(axis.ticks = element_blank()) +
  xlab("") + ylab("")
# add a single point

p <- p + geom_point(data=world.cities[],aes(long,lat),colour="red",size=2)
p


# data.pl$y <- rep(1,6)
# plot(data.pl, col = data.cl$cluster, main ="K-Means")
# points(data.cl$centers, pch =19 , cex =1.5 , col =1:clusters)

k_max <- 6
tot.withinss <- rep(0,k_max)
# compute the sum of intra-cluster errors W over k:1->k_max
for (i in 1:k_max){
  data.cl <- kmeans(world.cities[,7:9], i, algorithm = "Lloyd", iter.max = 10)
  tot.withinss[i] <-  data.cl$tot.withinss
}
plot(1:k_max, xlab = 'K',tot.withinss, ylab= 'Total within cluster variation', main= 'Total within cluster variation against K')

# compute the betweeness SS B over k:1->k_max
betweenss <- rep(0,k_max)
for (i in 1:k_max){
  data.cl <- kmeans(world.cities[,7:9], i, algorithm = "Lloyd", iter.max = 10)
  betweenss[i] <-  data.cl$betweenss
}
plot(1:k_max, xlab = 'K',betweenss, ylab= 'Between cluster sum of errors', main= 'Between cluster sum of squares against K')

# compute the CH index over k:1->k_max
ch <- rep(0,k_max)
n <- nrow(world.cities)
for (i in 2:k_max){
  ch[i] <- (betweenss[i]/(i-1)) / (tot.withinss[i]/(n-i))
}
plot(2:k_max, xlab = 'K',ch[2:k_max], ylab= 'CH', main= 'CH index against K')

clusters <- 2
data.cl <- kmeans(world.cities[,7:9], clusters, algorithm = "Lloyd", iter.max = 10)
# data.cl$cluster
world.cities$clusters <- data.cl$cluster


world<-map_data('world')
p <- ggplot(legend=FALSE) +
  geom_polygon( data=world, aes(x=long, y=lat,group=group)) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_blank(),axis.text.y = element_blank()) +
  theme(axis.ticks = element_blank()) +
  xlab("") + ylab("")
# add a single point

# world.cities<-world.cities[,4:5]

p <- p + geom_point(data=as.data.frame(world.cities[which(world.cities$clusters==1),]),aes(long,lat),colour="yellow",size=2) +
  geom_point(data=as.data.frame(world.cities[which(world.cities$clusters==2),]),aes(long,lat),colour="red",size=2) 
p



data(world.cities)
world.cities <- world.cities[which(world.cities$pop>1000000),]
R<-6371.0
world.cities$x <- R * cos(world.cities$lat) * cos(world.cities$long)
world.cities$y <- R * cos(world.cities$lat) * sin(world.cities$long)
world.cities$z <- R *sin(world.cities$lat)

# world.cities<-world.cities[,4:6]

data.cl <- pam(world.cities[,7:9], clusters, metric = 'manhattan')
world.cities$clusters <- data.cl$clustering

world<-map_data('world')
# sf<-data.frame(long=-122.26,lat=37.47)
p <- ggplot(legend=FALSE) +
  geom_polygon( data=world, aes(x=long, y=lat,group=group)) +
  theme(panel.background = element_blank()) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_blank(),axis.text.y = element_blank()) +
  theme(axis.ticks = element_blank()) +
  xlab("") + ylab("")
# add a single point

p <- p + geom_point(data=world.cities[which(world.cities$clusters==1),],aes(long,lat),colour="yellow",size=2) +
  geom_point(data=world.cities[which(world.cities$clusters==2),],aes(long,lat),colour="red",size=2)
p