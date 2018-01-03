library(catmaid)


aes_skids=as.integer(catmaid_fetch("/1/skeletons/?nodecount_gt=1&created_by=120"))
aesn=read.neurons.catmaid(aes_skids)

length(aesn)
head(aesn)
as.data.frame(aesn)
as.data.frame(neurons)

neuron=read.neuron.catmaid(30591)

class(neurons)
class(neuron)

plot3d(neurons, soma=2000, WithConnectors =T)
plot3d(aesn[1:10], soma=2000)

clear3d()

good_soma=sapply(aesn, function(x) !is.null(x$tags$soma))
good.neurons=aesn[good_soma]
plot3d(good.neurons, soma=2000, WithConnectors=T)

get.synapses.neuron<-function(neuron, direction = c(1,0)){
   neuron$connectors[,c("x","y","z")]
   neuron$connectors[neuron$connectors$prepost%in%direction,c("x","y","z")]
}

get.synapses.neuronlist<-function(neurons, direction= c(1,0)){
  points = nlapply(aesn, get.synapses.neuron, direction = direction)
  do.call(rbind, points)
}

fav = read.neurons.catmaid("name:LH Projection Neuron1806230 AES")
skid = names(fav)
connections=catmaid_query_connected(1806229)
head(connections$incoming)

neuronnames= catmaid_get_neuronnames(connections$incoming$partner)
inputs=cbind(connections$incoming, neuronnames)

if(!require("devtools"))install.packages("devtools")
devtools::install_github("easyGgplot2", "kassambara")
devtools::install_github("alexanderbates/catnat")

library(catnat)

install_github("jefferis/elmr")

library(elmr)
neurons.fcwb=xform_brain(aesn, sample=FAFB13, reference = FCWB)
neurons.fcwb=fetchn_fafb(aesn, mirror=F, reference =FCWB)

plot3d(neurons.fcwb)
plot3d(FCWBNP.surf)
plot3d(FCWBNP.surf, alpha=0.2)
clear3d()

LH.right=subset(FCWBNP.surf, "LH.R")
clear3d();plot3d(LH.right, alpha=0.5)
points3d(xyzmatrix(LH.right))

lhns=neurons.fcwb[sapply(neurons.fcwb, function(x) sum(pointsinside(xyzmatrix(x), LH.right))>100)]
plot3d(lhns, lwd=3, WithConnectors=T, soma =T)

pns= read.neurons.catmaid("annotation:^PN$")
pn=pns[1]
model=make.anatomical.model(pn, substrate="cable")
plot(model)

library(catnat)

get.synapses.neuron<-function(neuron, direction = c(1,0)){
  neuron$connectors[,c("x","y","z")]
  neuron$connectors[neuron$connectors$prepost%in%direction,c("x","y","z")]
}

n=neurons.inside(alpha=model, pns)
plot3d(n)

potential_pns<-function(neurons) {
  neurons.fcwb=xform_brain(neurons, sample=FAFB13, reference = FCWB)
  AL.right=subset(FCWBNP.surf, "AL.R")
  LH.right=subset(FCWBNP.surf, "LH.R")
  lhns=neurons[sapply(neurons.fcwb, function(x) sum(pointsinside(xyzmatrix(x), LH.right))>100)]
  alns=neurons[sapply(neurons.fcwb, function(x) sum(pointsinside(xyzmatrix(x), AL.right))>100)]
  neurons.fcwb[lhns%in%alns]
  }


library(elmr)
dps=read.neuronlistfh("http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/flycircuit/dpscanon.rds", localdir=getOption('flycircuit.datadir'))


neurons.dps=dotprops(neurons.fcwb, resample=1)

fromCatmaid=nblast(neurons.dps, normalised=T, target=dps)
fromTraces=nblast(neurons.dps, normalised=T, target=dps)
resultsAvg=(fromCatmaid)

install.packages(doMC); library(doMC); register(4)

results=nblast_fafb(names(aesn[1]), db=dps, mirror=F, normalised=T, parallel=T)
clear3d(); plot3d(results, hits=1)
plot3d(results)
