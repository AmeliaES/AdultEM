#log in to catmaid
catmaid_login()



# install
if (!require("devtools")) install.packages("devtools")
# nb repo is rcatmaid, but R package name is catmaid
devtools::install_github("jefferis/rcatmaid")

# use 
library(catmaid)

# general help starting point
?catmaid

# examples
example(catmaid_login)
example(catmaid_fetch)
example(catmaid_get_compact_skeleton)
example(catmaid_get_neuronnames)


# use with nat
library(nat)
nl=read.neurons.catmaid(c(10418394,4453485), pid=1)
open3d()
# nb this also plots the connectors (i.e. synapses) 
# red = presynapses, cyan = postsynapses
plot3d(nl, WithConnectors=TRUE)



# fetch olfactory receptor neurons
orns=read.neurons.catmaid("name:ORN", .progress='text')
# calculate some useful metadata
orns[,'Or']= factor(sub(" ORN.*", "", orns[,'name']))

# repeat for their PN partners, note use of search by annotation
pns=read.neurons.catmaid("annotation:PNs$", .progress='text')
pns[,'Or']= factor(sub(" PN.*", "", pns[,'name']))

lhns=read.neurons.catmaid("annotation:^LH LN$")

# plot, colouring by odorant receptor
plot3d(orns, col=Or)
# note that we plot somata with a radius of 1500 nm
plot3d(pns, col=Or, soma=1500)

lhns=read.neurons.catmaid("annotation:^LH LN$")
lhns[,'Or']= factor(sub(" LH.*", "", lhns[,'name']))



# find all the ORN downstream partners with at least 2 synapses
orn_partners=catmaid_query_connected(orns[,'skid'], minimum_synapses = 2)
# keep the ones not already in our set of PNs
# there are lots!
non_pn_downstream_ids=setdiff(unique(orn_partners$outgoing$partner), pns[,'skid'])
# download and plot those neurons
non_pn_downstream=read.neurons.catmaid(non_pn_downstream_ids, .progress='text')
plot3d(non_pn_downstream, col='grey', soma=1000)

# remove the last set of plotted neurons
npop3d()

## Plot, but colouring partners by number of synapses they receive from ORNs
# first collect those synapse numbers
library(dplyr)
totsyndf=orn_partners$outgoing %>% 
  group_by(partner) %>% 
  summarise(totsyn=sum(syn.count)) %>% 
  arrange(desc(totsyn))
hist(totsyndf$totsyn)
# now do the plot
clear3d()
# matlab style palette
jet.colors <-
  colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
# plot colouring by synapse number on a log scale 
# note that it is necessary to convert totsyndf$partner to a character
# vector to ensure that they are not treated as integer indices
plot3d(as.character(totsyndf$partner),  db=c(pns, non_pn_downstream), 
       col=jet.colors(10)[cut(totsyndf$totsyn, breaks = 2^(0:10))], soma=1000)

# Now let's cluster these other connected neurons
library(nat.nblast)
# convert to nblast-compatible format
# nb also convert from nm to um, resample to 1µm spacing and use k=5
# nearest neighbours of each point to define tangent vector
non_pn_downstream.dps=dotprops(non_pn_downstream/1e3, k=5, resample=1, .progress='text')
# now compute all x all NBLAST scores and cluster
non_pn_downstream.aba=nblast_allbyall(non_pn_downstream.dps, .progress='text')
non_pn_downstream.hc=nhclust(scoremat = non_pn_downstream.aba)
# plot result of clusterting as dendrogram, labelled by neuron name (rather than id)
plot(non_pn_downstream.hc, label=non_pn_downstream[,'name'])
# open new window
nopen3d()
# plot in 3d cutting into 2 clusters essentially left right
plot3d(non_pn_downstream.hc,db=non_pn_downstream, k=2, soma=1000)
clear3d() 
# 4 clusters - note local and projection neurons, gustatory neurons
plot3d(non_pn_downstream.hc,db=non_pn_downstream, k=4, soma=1000)