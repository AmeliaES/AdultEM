library(elmr)
library(catmaid)

#get neuron and find points in LH
neuron = read.neuron.catmaid(23829)
index = match(118714, neuron$d$PointNo)
neuron.distal = distal_to(neuron, index)
neuron.distal.points = neuron$d[neuron.distal,]

#select connectors in LH
connectors = neuron$connectors[neuron$connectors$treenode_id %in% neuron.distal.points$PointNo,]
#for downstream partners - outgoing connections only
connectors.outgoing = connectors[connectors$prepost == 0,]
connectors.incoming = connectors[connectors$prepost == 1,]
#randomise outgoing connectors for tracing downstream partners
perm = connectors.outgoing[sample(nrow(connectors.outgoing)),]

#—URL generator—
connector_URL <- function(dfrow){
  base = "https://neuropil.janelia.org/tracing/fafb/v13"
  catmaid_url = paste0(base, "?pid=1")
  catmaid_url = paste0(catmaid_url, "&zp=", dfrow[,"z"])
  catmaid_url = paste0(catmaid_url, "&yp=", dfrow[,"y"])
  catmaid_url = paste0(catmaid_url, "&xp=", dfrow[,"x"])
  catmaid_url = paste0(catmaid_url, "&tool=tracingtool")
  catmaid_url = paste0(catmaid_url, "&active_skeleton_id=23829")
  catmaid_url = paste0(catmaid_url, "&active_node_id=", dfrow[,"connector_id"])
  catmaid_url = paste0(catmaid_url, "&sid0=9&s0=0")
  
  invisible(catmaid_url)
}
#———

#generate URLs for each row
perm$URL = character(nrow(perm))
perm[,"URL"] = sapply(1:nrow(perm), function(x) perm[x, "URL"] = connector_URL(perm[x,]))

#write out as CSV to save in Google docs
write.csv(perm, file = "DL4PN_outgoing_connectors_in_LH_R.csv")

#plot to check locations of connectors, and check that all outgoing are included the randomised set
nopen3d()
plot3d(neuron, WithNodes = FALSE)
points3d(xyzmatrix(connectors.outgoing), col = 'red')
points3d(xyzmatrix(connectors.incoming), col = 'blue')
points3d(xyzmatrix(perm), col = 'yellow', size = '8')