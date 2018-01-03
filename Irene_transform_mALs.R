library(catmaid)
library(nat.nblast)
setwd("~/projects/mAL/")
# Clustering session started with Greg 2017-07-28 (evening!)
# Cluster by whole neuron 
mAL = read.neurons.catmaid("annotation:IV mAL lineage")
# convert to microns and resample to 1µm segments
mAL.dps = dotprops(mAL/1e3, resample=1, k=5)
mAL.aba = nblast_allbyall(mAL.dps, UseAlpha = T)
mAL.hclust = nhclust(scoremat = mAL.aba)
plot(mAL.hclust)
plot(mAL.hclust, labels = mAL[,'name'] )
clear3d()
plot3d(mAL.hclust, db=mAL, k=3)
### Some stuff to merge tracing docs (Google) with mAL neuron list (R)
# Read in google sheet
library(googlesheets)
tdi.gs=gs_title("Tracing documentation Irene")
tdi=gs_read(tdi.gs)
# find entity ids embedded in name
mAL[,'nameid']=sub('.*?([0-9]+).*','\\1', mAL[,'name'])
m=merge(mAL[,], tdi[,c('Name ID', 'Morphology')], by.x='nameid', by.y='Name ID')
rownames(m)=names(mAL)
mAL[,]=m
###
plot3d(mAL, Morphology=='Ring')
clear3d()
plot3d(mAL, grepl('Ring',Morphology))
plot(mAL.hclust, labels = mAL[,'Morphology'] )
clear3d()
plot3d(mAL.dps)
###
pdf('ClusterAll.pdf', width =10, height = 8)
plot(mAL.hclust, labels = paste(mAL[,'Morphology'],mAL[,'nameid']) , main = 'Cluster Whole Neuron')
dev.off()
### Clustering with Dorsal part (i.e. Axon arbour) alone
library(elmr)
prune_dorsal <- function(n){ 
  print(n$skid)
  node_id = n$tags$Dorsal
  dors = distal_to( n, node.pointno = node_id )
  ss = subset( n, dors )
  ss
}
mAL.d=nlapply(mAL, prune_dorsal)
mAL.d.dps = dotprops(mAL.d/1e3, k=5, resample=1)
mAL.d.aba = nblast_allbyall(mAL.d.dps, UseAlpha = T)
mAL.d.hclust = nhclust(scoremat = mAL.d.aba)
plot(mAL.d.hclust)
plot(mAL.d.hclust, labels = mAL[,'name'] , main = 'Cluster by Axons')
plot(mAL.d.hclust, labels = mAL[,'Morphology'], main = 'Cluster by Axons')
pdf('ClusterAxons.pdf', width =10, height = 8)
plot(mAL.d.hclust, labels = paste(mAL[,'Morphology'],mAL[,'nameid']) , main = 'Cluster by Axons')
dev.off()
clear3d();plot3d(mAL.d.hclust, k=3, db=mAL)
clear3d();plot3d(mAL.d.hclust, k=7, db=mAL)
for(i in 1:7){
  clear3d()
  plot3d(mAL.d.hclust, k=7, db=mAL, groups = i, lwd=2)
  plot3d(mAL.d.hclust, k=7, db=mAL, groups = setdiff(1:7,i), col='grey')
  readline(prompt = 'Press Return to continue')
}
for(i in 1:7){
  clear3d()
  plot3d(mAL.d.hclust, k=7, db=mAL, groups = i)
  plot3d(mAL.d.hclust, k=7, db=mAL, groups = setdiff(1:7,i), col='grey')
  snapshot3d(paste0('mAL-byaxon-group-',i,'.png'))
}
for(i in 1:7){
  clear3d()
  plot3d(mAL.hclust, k=7, db=mAL, groups = i)
  plot3d(mAL.hclust, k=7, db=mAL, groups = setdiff(1:7,i), col='grey')
  snapshot3d(paste0('mAL-bywhole-group-',i,'.png'))
}
## Compare both clusterings
library(dendextend)
tanglegram(mAL.hclust, mAL.d.hclust)
### Could repeat after adding a tag for the Ventral (Dendritic) part of the neuron