#Code to show neuron of interest as a 3d plot, 
#with synapses in different colours corresponding to the neurons it is connected to.


#get neuron of interest 
MVP2 = read.neuron.catmaid(2333007)
VPM3 = read.neuron.catmaid(1329078)
VPM4 = read.neuron.catmaid(1191261)

#Generate connector IDs, skids ect. upstream of neuron of interest
neuron_data<-catmaid_get_connectors_between(post_skids = 2333007)

#Plots all synapses of MVP2
nopen3d()
plot3d(MVP2)
points3d(neuron_data[,c('connector_x','connector_y','connector_z')], col="blue")

#Now select out of the list of pre_skids for those only containing certain skids, 
selected_pre_skids_VPM3 <-neuron_data[neuron_data$pre_skid %in% 1329078,]
selected_pre_skids_VPM4 <-neuron_data[neuron_data$pre_skid %in% 1191261,]
plot3d(MVP2, col="grey", soma=2000)
points3d(selected_pre_skids_VPM3[,c('connector_x','connector_y','connector_z')], col="dark green", size=7)
points3d(selected_pre_skids_VPM4[,c('connector_x','connector_y','connector_z')], col="purple", size=7)




#Next, we will look at how to have more than one skid by making it into a function.



interesting_skids <- function(skid, pre.skid1, pre.skid2) {
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

#This does not work
#all.pre.skids = sapply(pre.skids, function(x) neuron_data[neuron_data$pre_skid %in% x,])
#nopen3d()
#plot3d(neuron)
#points3d(all.pre.skids[,c('connector_x','connector_y','connector_z')])
#test
#interesting_skids3(2333007, c(1191261, 540361))

#This does not work
interesting_skids <- function(skid, pre.skid1, pre.skid2) {
  #get neuron of interest (skid)
  neuron = read.neuron.catmaid(skid)
  neuron_data = catmaid_get_connectors_between(post_skids = skid)
  
  all.pre.skids=sapply(pre.skid1, pre.skid2, function(x) neuron_data[neuron_data$pre_skid %in% x])
  points3d(all.pre.skids[,c('connector_x','connector_y','connector_z')], col="purple", size=7)
  nopen3d()
  plot3d(neuron, soma=2000, col='Grey')
  
}







#Also need to assign a legend for each colour to match each neuron


