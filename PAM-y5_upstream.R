#gives random ordering of pre-synaptic profiles
library(elmr)
library(catmaid)
skid <- 3643424
n <- read.neurons.catmaid(skid)
#function to return the incoming pre-synaptic nodes for the selected skid proximal of the provided cut point

incoming_connections <- function(skid) {
  neuron = read.neuron.catmaid(skid)
  SMPindex <- match(neuron$tags$AJES_SMPproximal, neuron$d$PointNo)
  SMProot <- match(neuron$tags$AJES_SMP_root, neuron$d$PointNo)
  neuron.distal = distal_to(neuron, node.idx = SMPindex, root.idx = SMProot)
  neuron.distal.points = neuron$d[neuron.distal,]
  
  #debug- graph to check selected distal points are in correct region
  nopen3d()
  plot3d(neuron, col = "gray23")
  points3d(neuron.distal.points[,c('X','Y','Z')], col = "deepskyblue")
  
  all_connectors <- catmaid_get_connectors_between(post_skids = skid)
  connectors = all_connectors[all_connectors$post_node_id %in% neuron.distal.points$PointNo,]
  #debug - graph to check all selected connectors in correct region
  nopen3d()
  plot3d(neuron, col = "black")
  points3d(connectors[,c('connector_x','connector_y','connector_z')])
  
  return(connectors)
}

#get incoming synapses 
n_incoming <- incoming_connections(skid = skid)
#randomise outgoing connectors for tracing downstream partners
perm = n_incoming[sample(nrow(n_incoming)),]
#debug - 
nopen3d()
plot3d(n)
points3d(n_incoming[,c('post_node_x','post_node_y','post_node_z')], col='blue')
points3d(perm[,c('post_node_x','post_node_y','post_node_z')], col = 'green', size = 8)
#—URL generator—
connector_URL <- function(dfrow){
  base = "https://neuropil.janelia.org/tracing/fafb/v14/"
  catmaid_url = paste0(base, "?pid=1")
  catmaid_url = paste0(catmaid_url, "&zp=", dfrow[,"pre_node_z"])
  catmaid_url = paste0(catmaid_url, "&yp=", dfrow[,"pre_node_y"])
  catmaid_url = paste0(catmaid_url, "&xp=", dfrow[,"pre_node_x"])
  catmaid_url = paste0(catmaid_url, "&tool=tracingtool")
  catmaid_url = paste0(catmaid_url, "&active_skeleton_id=", dfrow[,'pre_skid'])
  catmaid_url = paste0(catmaid_url, "&active_node_id=", dfrow[,"pre_node_id"])
  catmaid_url = paste0(catmaid_url, "&sid0=5&s0=0")
  
  invisible(catmaid_url)
}
#———
#generate URLs for each row
perm$URL = character(nrow(perm))
perm[,"URL"] = sapply(1:nrow(perm), function(x) perm[x, "URL"] = connector_URL(perm[x,]))
#write out as CSV to save in Google docs
write.csv(perm, file = 'PAM-y5(7)_upstream_SMP.csv')

