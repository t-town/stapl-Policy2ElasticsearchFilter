library(RJSONIO)
options(scipen=5)
#/** for the increasing number of arguments**/
json1 <- fromJSON(file("results/10000/FilterOutput.dat"))
means1 <- data.matrix(json1["mean"])
plot(means1)
par(new=T)

json2 <- stream_in(file("results/10000/OriginalOutput.dat"))
means2 <- data.matrix(json2["mean"])
plot(means2)
meanDif = means2-means1
m = max(max(means1),max(means2))
plot(means2,ylim=c(0,m))
points(means1)

#for the increasing number of documents
#Results with filter:
filterResults <- list()
#Naive brute force results
naiveResults <- list()

f = file("results/10000/FilterOutput.dat")
json1 <- fromJSON(f)
close(f)
f = file("results/10000/OriginalOutput.dat")
json2 <- fromJSON(f)
close(f)
xmin = min(min(json1["totals"][[1]]),min(json2["totals"][[1]]))
xmax = max(max(json1["totals"][[1]]),max(json2["totals"][[1]]))
hist(as.numeric(json1["totals"][[1]]),col = rgb(1,0,0,0.5),xlim=c(xmin,xmax))
hist(as.numeric(json2["totals"][[1]]),col=rgb(0,0,1,0.5),add=T)

filterResults$r10 <- json1
naiveResults$r10 <- json2


f = file("results/100000/FilterOutput.dat")
json1 <- fromJSON(f)
close(f)
f = file("results/100000/OriginalOutput.dat")
json2 <- fromJSON(f)
close(f)
xmin = min(min(json1["totals"][[1]]),min(json2["totals"][[1]]))
xmax = max(max(json1["totals"][[1]]),max(json2["totals"][[1]]))
hist(as.numeric(json1["totals"][[1]]),col = rgb(1,0,0,0.5),xlim=c(xmin,xmax))
hist(as.numeric(json2["totals"][[1]]),col=rgb(0,0,1,0.5),add=T)

filterResults$r100 <- json1
naiveResults$r100 <- json2

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
hist(as.numeric(json1["serverDuration"][[1]]),col = rgb(1,0,0,0.5),xlim=c(xmin,xmax))
hist(as.numeric(json2["serverDuration"][[1]]),col=rgb(0,0,1,0.5),add=T)

par(mfrow=c(1,1))
xmin = min(min(json1["otherDuration"][[1]]),min(json2["otherDuration"][[1]]))
xmax = max(max(json1["otherDuration"][[1]]),max(json2["otherDuration"][[1]]))
hist(as.numeric(json1["otherDuration"][[1]]),col = rgb(1,0,0,0.5),xlim=c(xmin,xmax))
hist(as.numeric(json2["otherDuration"][[1]]),col=rgb(0,0,1,0.5),add=T)

#evolution:
values = c(10000,100000)
naiveMeans = c(naiveResults$r10$mean, naiveResults$r100$mean)
filterMeans = c(filterResults$r10$mean, filterResults$r100$mean)
ymin = min(min(naiveMeans),min(filterMeans))
ymax = max(max(naiveMeans),max(filterMeans))
xmin = min(values)
xmax = max(values)
plot(values,naiveMeans,
     xlim = c(xmin,xmax),
     ylim = c(ymin,ymax),
     xlab = "# resources",
     ylab = "time in microseconds",
     type="l",
     xaxt="n",
     col = "blue")
axis(1,at=values,labels=values)

par(new=T)
plot(values,filterMeans, xlim = c(xmin,xmax), ylim = c(ymin,ymax), xlab = '', ylab = '',type='l', xaxt = "no", yaxt = "no", col = "red")
par(new=F)
