#Visualise synapses on a neuron of interest, where synapses are coloured differently for each different neuron connecting.

#With this function you give it the skid of your main interesting neuron,
#and 2 other skids upstream of your main neuron, the synapses it makes onto your neuron of interest will show in different colours.
#This is only for input synapses onto your neuron of interest.

#Problems:
#Only 2 skids, only incoming synapses, no legend.
#Tried to use an apply function but it didn't work. 

#Read neurons that you are interested in, firstly the main neuron then two neurons you want to view the synapses of...
main_neuron=read.neuron.catmaid(2333007)
n1=read.neuron.catmaid(1329078)
n2=read.neuron.catmaid(1191261)

#' Title
#'
#' @param skid 
#' @param pre.skid1 
#' @param pre.skid2 
#'
#' @return
#' @export
#'
#' @examples
synapses <- function(skid, pre.skid1, pre.skid2) {
  #get neuron of interest (skid)
  neuron = read.neuron.catmaid(skid)
  neuron_data = catmaid_get_connectors_between(post_skids = skid)
  
  selected_pre_skids_1 <-neuron_data[neuron_data$pre_skid %in% pre.skid1,]
  selected_pre_skids_2 <-neuron_data[neuron_data$pre_skid %in% pre.skid2,]
  
  nopen3d()
  plot3d(neuron, soma=2000, col='Grey')
  points3d(selected_pre_skids_1[,c('connector_x','connector_y','connector_z')], col="dark green", size=7)
  points3d(selected_pre_skids_2[,c('connector_x','connector_y','connector_z')], col="purple", size=7)
  
}

synapses(main_neuron, n1, n2)
synapses(2333007, 1329078, 1191261)


#try using catmaid_get_connectors function

main_neuron <- read.neuron.catmaid(2333007)
connector_table <- connectors(main_neuron)

#Get skids synapsing onto neuron of interest
incoming_connector_table <- connector_table[connector_table$prepost %in% 1,]
incoming_connector_ID <- incoming_connector_table$connector_id
incoming_skid_table <- catmaid_get_connectors(incoming_connector_ID)
incoming_skids <- incoming_skid_table$pre

#Get skids recieving input from neuron of interst
outgoing_connector_table <- connector_table[connector_table$prepost %in% 0,]
outgoing_connector_ID <- outgoing_connector_table$connector_id
outgoing_skid_table <- catmaid_get_connectors(outgoing_connector_ID)
outgoing_skids <- outgoing_skid_table$post

length(which(incoming_skids == "1191261"))
#problem with my incoming_skid list is that it gives the same skid more than once for one connector,
#so is not a true representative of how many connections one skid makes with a neuron

synapses<- function(seed, queries){
 neuron = read.neuron.catmaid(seed)
 pre_skid_data = catmaid_get_connectors_between(post_skids = seed)
 pre_connectors <- lapply(queries, function(neuron,connectors)  connectors[,c('connector_x','connector_y','connector_z')] )
  
}
  
 lapply(queries, get_post_xyz, connectors= pre_synapses, prepost = 'pre')
 get_post_xyz <- function(neuron, connectors, prepost) {
  xyz <- connectors[,c('post_node_x','post_node_y','post_node_z')]
  #print(str(xyz))
  if (prepost == 'pre') {
    xyz <- xyz[connectors$post_skid == neuron,]
  } else if (prepost == 'post') {
    xyz <- xyz[connectors$pre_skid == neuron,]
  }
  return(xyz)
}
 
  nopen3d()
  plot3d(neuron, soma=2000, col='Grey')
  points3d(selected_pre_skids_1[,c('connector_x','connector_y','connector_z')], col="dark green", size=7)
  points3d(selected_pre_skids_2[,c('connector_x','connector_y','connector_z')], col="purple", size=7)


