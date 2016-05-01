library(RJSONIO)
options(scipen=5)

#/** for the increasing number of arguments**/
#Results with filter:

f = file("results/many/0/FilterOutput.dat")
filter0 <- fromJSON(f)
close(f)
f = file("results/many/0/OriginalOutput.dat")
naive0 <- fromJSON(f)
close(f)

f = file("results/many/1/FilterOutput.dat")
filter1 <- fromJSON(f)
close(f)
f = file("results/many/1/OriginalOutput.dat")
naive1 <- fromJSON(f)
close(f)

f = file("results/many/2/FilterOutput.dat")
filter2 <- fromJSON(f)
close(f)
f = file("results/many/2/OriginalOutput.dat")
naive2 <- fromJSON(f)
close(f)

f = file("results/many/3/FilterOutput.dat")
filter3 <- fromJSON(f)
close(f)
f = file("results/many/3/OriginalOutput.dat")
naive3 <- fromJSON(f)
close(f)

f = file("results/many/4/FilterOutput.dat")
filter4 <- fromJSON(f)
close(f)
f = file("results/many/4/OriginalOutput.dat")
naive4 <- fromJSON(f)
close(f)

f = file("results/many/5/FilterOutput.dat")
filter5 <- fromJSON(f)
close(f)
f = file("results/many/5/OriginalOutput.dat")
naive5 <- fromJSON(f)
close(f)

mapf <- function(lst) {
  return(mean(lst["totals"][[1]]))
} 
plotIncreasing <- function(filter, naive, subtitle) {
  means1 = unlist(Map(mapf,filter))
  means2 = unlist(Map(mapf,naive))
  ymin = min(min(means1),min(means2))
  ymax = max(max(means1),max(means2))
  #1 million micro seconds = 1 second
  plot(1:50,means1,ylim=c(0,160000),type="p",col="blue",xlab = "# arguments", ylab = "time in microseconds", main = paste("Increasing amount of resource attributes\n processing 1000 documents\n",subtitle))
  par(new=T)
  plot(1:50,means2,col="red",type="p",ylim=c(0,180000),xlab = '', ylab = '',xaxt = "no", yaxt = "no")
  par(xpd=TRUE)
  legend("top",c("new implementation","naive implementation"),col = c("blue","red"),pch="o")
  
}
par(mfrow=c(2,3))
par(mfrow=c(1,1))
plotIncreasing(filter0,naive0,"100 % of documents allowed")
par(new=T)
plotIncreasing(filter1,naive1,"")
par(new=T)
plotIncreasing(filter2,naive2,"")
par(new=T)
plotIncreasing(filter3,naive3,"")
par(new=T)
plotIncreasing(filter4,naive4,"")
par(new=T)
plotIncreasing(filter5,naive5,"")


#for the increasing number of documents
#Results with filter:
filterResults <- list()
#Naive brute force results
naiveResults <- list()

f = file("results/5000/FilterOutput.dat")
json1 <- fromJSON(f)
close(f)
f = file("results/5000/OriginalOutput.dat")
json2 <- fromJSON(f)
close(f)

filterResults$r5 <- json1
naiveResults$r5 <- json2

f = file("results/10000/FilterOutput.dat")
json1 <- fromJSON(f)
close(f)
f = file("results/10000/OriginalOutput.dat")
json2 <- fromJSON(f)
close(f)

filterResults$r10 <- json1
naiveResults$r10 <- json2

f = file("results/20000/FilterOutput.dat")
json1 <- fromJSON(f)
close(f)
f = file("results/20000/OriginalOutput.dat")
json2 <- fromJSON(f)
close(f)

filterResults$r20 <- json1
naiveResults$r20 <- json2

f = file("results/50000/FilterOutput.dat")
json1 <- fromJSON(f)
close(f)
f = file("results/50000/OriginalOutput.dat")
json2 <- fromJSON(f)
close(f)
#xmin = min(min(json1["totals"][[1]]),min(json2["totals"][[1]]))
#xmax = max(max(json1["totals"][[1]]),max(json2["totals"][[1]]))
#hist(as.numeric(json1["totals"][[1]]),col = rgb(1,0,0,0.5),xlim=c(xmin,xmax))
#hist(as.numeric(json2["totals"][[1]]),col=rgb(0,0,1,0.5),add=T)

filterResults$r50 <- json1
naiveResults$r50 <- json2


f = file("results/100000/FilterOutput.dat")
json1 <- fromJSON(f)
close(f)
f = file("results/100000/OriginalOutput.dat")
json2 <- fromJSON(f)
close(f)

filterResults$r100 <- json1
naiveResults$r100 <- json2

f = file("results/500000/FilterOutput.dat")
json1 <- fromJSON(f)
filterResults$r500 <- json1

#evolution:
par(mfrow=c(1,1))
values = c(5000,10000,20000,50000,100000)
naiveValues = list(naiveResults$r5$totals,naiveResults$r10$totals,naiveResults$r20$totals,naiveResults$r50$totals,naiveResults$r100$totals)
filterValues = list(filterResults$r5$totals,filterResults$r10$totals,filterResults$r20$totals,filterResults$r50$totals,filterResults$r100$totals)
mapf <- function(list) {
  return(mean(list))
}
naiveMeans = unlist(Map(mapf,naiveValues))
filterMeans = unlist(Map(mapf,filterValues))
ymin = min(min(naiveMeans),min(filterMeans))
ymax = max(max(naiveMeans),max(filterMeans))
xmin = min(values)
xmax = max(values)
#means
plot(values,filterMeans,
     xlim = c(xmin,xmax),
     ylim = c(ymin,ymax),
     xlab = "number of resources in database",
     ylab = "time in microseconds",
     type="l",
     xaxt="n",
     col = "blue",
     main = "Total processing time to retrieve all allowable resources")
axis(1,at=values,labels=values)

par(new=T)
values2 = c(5000,10000,20000,50000,100000)
plot(values2,naiveMeans, xlim = c(xmin,xmax), ylim = c(ymin,ymax), xlab = '', ylab = '',type='l', xaxt = "no", yaxt = "no", col = "red")
par(new=F)
legend(x = xmin, y= ymax, c("new implementation","naive implementation"),col = c("blue","red"),lty = c(1,1))

boxplot(filterValues,main = "boxplot of total duration of new implementation",names = values, ylab = "time in microseconds", xlab = "number of resources in database",col = "blue")
boxplot(naiveValues,main = "boxplot of total duration of naive implementation",names = values, ylab = "time in microseconds", xlab = "number of resources in database",col = "red")#, #add = T)

xmin = min(min(unlist(naiveValues[2])),min(unlist(filterValues[2])))
xmax = max(max(unlist(naiveValues[2])),max(unlist(filterValues[2])))

hist(unlist(naiveValues[2]),col = rgb(1,0,0,0.5),xlim=c(xmin,xmax), xlab = "processing time in microseconds", main = "histogram of total processing time to retrieve all allowable resources")
hist(unlist(filterValues[2]),col=rgb(0,0,1,0.5),add=T)
legend(x = xmax*0.75,y=300,c("new implementation","naive implementation"),col = c("blue","red"),lty = c(1,1))

#only Server
mapf <- function(list) {
  return(mean(list))
}
naiveServerValues = list(naiveResults$r5$serverDuration,naiveResults$r10$serverDuration,naiveResults$r50$serverDuration,naiveResults$r100$serverDuration)
filterServerValues = list(filterResults$r5$serverDuration,filterResults$r10$serverDuration,filterResults$r50$serverDuration,filterResults$r100$serverDuration)
naiveServerMeans = unlist(Map(mapf, naiveServerValues))
filterServerMeans = unlist(Map(mapf, filterServerValues))
ymin = min(min(naiveServerMeans),min(filterServerMeans))
ymax = max(max(naiveServerMeans),max(filterServerMeans))
xmin = min(values)
xmax = max(values)

plot(values,filterServerMeans,
     xlim = c(xmin,xmax),
     ylim = c(ymin,ymax),
     xlab = "Number of resources in database",
     ylab = "time in microseconds",
     type="l",
     xaxt="n",
     col = "blue",
     main = "Elasticsearch processing time to retrieve all allowable resources"
)

axis(1,at=values,labels=values)

par(new=T)
plot(values,naiveServerMeans, xlim = c(xmin,xmax), ylim = c(ymin,ymax), xlab = '', ylab = '',type='l', xaxt = "no", yaxt = "no", col = "red")
par(new=F)
legend(x = xmin, y= ymax, c("new implementation","naive implementation"),col = c("blue","red"),lty = c(1,1))

boxplot(filterServerValues,main = "boxplot of server duration of new implementation",names = values, ylab = "time in microseconds", xlab = "number of resources in database",col = "blue")
boxplot(naiveServerValues,main = "boxplot of server duration of naive implementation",names = values, ylab = "time in microseconds", xlab = "number of resources in database",col = "red")#, #add = T)

xmin = min(min(unlist(naiveServerValues[2])),min(unlist(filterServerValues[2])))
xmax = max(max(unlist(naiveServerValues[2])),max(unlist(filterServerValues[2])))

hist(unlist(naiveServerValues[2]),col = rgb(1,0,0,0.5),xlim=c(xmin,xmax), xlab = "processing time in microseconds", main = "histogram of Elasticsearch processing time to retrieve all allowable resources")
hist(unlist(filterServerValues[2]),col=rgb(0,0,1,0.5),add=T)
legend(x = xmax*0.75,y=300,c("new implementation","naive implementation"),col = c("blue","red"),lty = c(1,1))

#stacked representation
par(mfrow=c(1,2))

ymin = min(min(json1["totals"][[1]]),min(json2["totals"][[1]]))
ymax = max(max(json1["totals"][[1]]),max(json2["totals"][[1]]))
dd = t(data.matrix(data.frame(json1["otherDuration"][[1]][1:100], json1["serverDuration"][[1]][1:100])))
rownames(dd) <- c("Filter calculation","server calculation")
barplot(dd, col = c("blue","red"), legend = rownames(dd),ylim=c(ymin,ymax))
dd = t(data.matrix(data.frame(json2["otherDuration"][[1]][1:100], json2["serverDuration"][[1]][1:100])))
rownames(dd) <- c("Stapl calculation","server calculation")
barplot(dd, col = c("blue","red"), legend = rownames(dd),ylim = c(ymin,ymax))

#only server / only Filter
par(mfrow=c(1,1))
xmin = min(min(json1["serverDuration"][[1]]),min(json2["serverDuration"][[1]]))
xmax = max(max(json1["serverDuration"][[1]]),max(json2["serverDuration"][[1]]))
hist(as.numeric(json1["serverDuration"][[1]]),col = rgb(0,0,1,0.5),xlim=c(xmin,xmax),xlab = "Duration in microseconds",main="Elasticsearch query duration")
hist(as.numeric(json2["serverDuration"][[1]]),col=rgb(1,0,0,0.5),add=T)

par(mfrow=c(1,1))
xmin = min(min(json1["otherDuration"][[1]]),min(json2["otherDuration"][[1]]))
xmax = max(max(json1["otherDuration"][[1]]),max(json2["otherDuration"][[1]]))
hist(as.numeric(json1["otherDuration"][[1]]),col = rgb(1,0,0,0.5),xlim=c(xmin,xmax))
hist(as.numeric(json2["otherDuration"][[1]]),col=rgb(0,0,1,0.5),add=T)
