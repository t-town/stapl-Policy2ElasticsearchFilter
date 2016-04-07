library(jsonlite)
json1 <- stream_in(file("FilterOutput.dat"))
means1 <- data.matrix(json1["mean"])
plot(means1)
par(new=T)

json2 <- stream_in(file("OriginalOutput.dat"))
means2 <- data.matrix(json2["mean"])
plot(means2)
meanDif = means2-means1
m = max(max(means1),max(means2))
plot(means2,ylim=c(0,m))
points(means1)
