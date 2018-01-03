library(catmaid)
neurons=read.neurons.catmaid("name:AJES", OmitFailures = T)
aes_skids=as.integer(catmaid_fetch("/1/skeletons/?nodecount_gt=1&created_by=120"))
aesn=read.neurons.catmaid(aes_skids)

length(aesn)
head(neurons)
as.data.frame(aesn)

aesn.fav=subset(aesn, grepl("AES", name))

plot3d(aesn, soma=2000, WithConnectors = T)

good_soma=sapply(aesn, function(x) !is.null(x$tags$soma))
good.neurons=neurons[good_soma]
clear3d()
plot3d(good.neurons, soma=2000)

node.data=aesn[[1]]$d
points=aesn[[1]]$d[,c("X","Y","Z")]
points=node.data[,c("X","Y","Z")]

points3d(points, col='red')

3d.points=xyzmatrix(aesn)
points3d(3d.points, col='red')