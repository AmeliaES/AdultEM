library(flycircuit)
library(elmr)
library(doMC)
library(catmaid)

gmrdps<-read.neuronlistfh("http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/gmrdps/gmrdps.rds", localdir=getOption('flycircuit.datadir'))


source('~/.Rprofile')


s<- catmaid_skids("annotation:NAMK_putative_PAM-y5_RIGHT")
nam <- catmaid_get_neuronnames(s)
nam
results=nblast_fafb(5937017, mirror = TRUE)
summary(results) 
clear3d()
plot3d(results, soma = TRUE)
plot3d(results, hits = 1, soma = TRUE)



fc_neuron("FruMARCM-M001152_seg001")


s[[12]]
nam[12]
mal <- fc_neuron_type(regex = "aDT-b")
n <- fetchn_fafb(s, mirror = TRUE)
plot3d(n, col = "black", soma = TRUE)
clear3d()
plot3dfc(names(mal), soma = TRUE)

nopen3d()
clear3d()
plot3d(n[[11]], col = "darkred", soma = TRUE)
plot3dfc(names(mal)[71], soma = TRUE, col = "red")
plot3d(n[[13]], col = "darkblue", soma = TRUE)
plot3dfc(names(mal)[81], soma = TRUE, col =  "blue")
blue <- catmaid_get_neuronnames(s[[13]])
red <- catmaid_get_neuronnames(s[[11]])
bluefc<- names(mal)[81]
redfc <- names(mal)[71]
bluefc
redfc

asp13 <- fc_neuron_type(regex = "aSP-13")


#To Get soma:
  n1191261=read.neuron.catmaid(1652610)
plot3d(n1191261, soma=2000)

neurons <- annotation

#source your catmaid profile - change filename as needed
source('catmaid_log.R')

#blast neuron of interest against flycircuit db - insert skeleton_id as needed
results = nblast_fafb( skeleton_id )

#this returns a table with the first couple hits
summary(results)

#plot hits - change number as needed
plot3d(results, hits = 1)

#save a screenshot - change directory as needed
snapshot3d('/Users/philipps/Downloads/nblast.png')

#nblast returns gene names but for virtual fly brain we need neuron names - change neuron names as needed
library(flycircuit)
fc_neuron("FruMARCM-F001749_seg001")

#fc_neuron converts gene name into neuron name which you can take to VFB and check for e.g. publications

(fcidtable == "dopamin")


fc_gene_name("FBbt_00111015")


