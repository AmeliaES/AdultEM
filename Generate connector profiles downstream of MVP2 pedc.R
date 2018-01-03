#gives random ordering of post-synaptic profiles
library(elmr)
library(catmaid)
#skid of my neuron of interest, to look at it's downstream connections in a certain area (MVP2)
complete_MVP2 <- read.neurons.catmaid(2333007)
#function to return the outgoing connectors for the selected skid downstream of the provided cut point

outgoing_connections <- function(skid, cut_point) {
  #get neuron of interest (MVP2) and find connectors in arbour of interest (in pedc)
  neuron = read.neuron.catmaid(skid)
  index = match(cut_point, neuron$d$PointNo)
  neuron.distal = distal_to(neuron, index)
  neuron.distal.points = neuron$d[neuron.distal,]
  #debug- graph to check selected distal points are in correct region
  nopen3d()
  plot3d(neuron, col = "gray23")
  points3d(neuron.distal.points[,c('X','Y','Z')], col = "deepskyblue")
  
  #select outgoing connectors in arbor in pedc for MVP2
  all_connectors <- catmaid_get_connectors_between(pre_skids = skid)
  connectors = all_connectors[all_connectors$pre_node_id %in% neuron.distal.points$PointNo,]
  #debug - graph to check all selected connectors in correct region
  nopen3d()
  plot3d(neuron, col = "black")
  points3d(connectors[,c('connector_x','connector_y','connector_z')])
  
  return(connectors)
}

#get outgoing synapses 
outgoing_MVP2 <- outgoing_connections(2333007, 6143288)
#randomise outgoing connectors for tracing downstream partners
perm = outgoing_MVP2[sample(nrow(outgoing_MVP2)),]
#debug - 
nopen3d()
plot3d(complete_MVP2)
points3d(outgoing_MVP2[,c('post_node_x','post_node_y','post_node_z')], col='blue')
points3d(perm[,c('post_node_x','post_node_y','post_node_z')], col = 'green', size = 8)
#—URL generator—
connector_URL <- function(dfrow){
  base = "https://neuropil.janelia.org/tracing/fafb/v14/"
  catmaid_url = paste0(base, "?pid=1")
  catmaid_url = paste0(catmaid_url, "&zp=", dfrow[,"post_node_z"])
  catmaid_url = paste0(catmaid_url, "&yp=", dfrow[,"post_node_y"])
  catmaid_url = paste0(catmaid_url, "&xp=", dfrow[,"post_node_x"])
  catmaid_url = paste0(catmaid_url, "&tool=tracingtool")
  catmaid_url = paste0(catmaid_url, "&active_skeleton_id=", dfrow[,'post_skid'])
  catmaid_url = paste0(catmaid_url, "&active_node_id=", dfrow[,"post_node_id"])
  catmaid_url = paste0(catmaid_url, "&sid0=5&s0=0")
  
  invisible(catmaid_url)
}
#———
#generate URLs for each row
perm$URL = character(nrow(perm))
perm[,"URL"] = sapply(1:nrow(perm), function(x) perm[x, "URL"] = connector_URL(perm[x,]))
#write out as CSV to save in Google docs
write.csv(perm, file = 'test.csv')



