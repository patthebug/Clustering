
require(cluster)

# Example 1: Fibonacci sequence)
data<- data.frame('x'= c(-4,0,-0.01,0,0.02,0,11,0,3,0,5,0))

ag <- agnes(data, method="average", stand=F)
summary(ag)
plot(ag,labels=data$x)

data<- data.frame('x'= c(-4,0,-0.01,0,0.02,0,11,0,3,0,5,0))

ag <- agnes(data, method="median", stand=F)
summary(ag)
plot(ag,labels=data$x)