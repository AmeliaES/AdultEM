#calculate the number of non-MVP2 synapses that occur distal to MVP2 synapses and the root

skid = 4291899
input.skid = 2333007
n <- read.neuron.catmaid(skid)

#find node IDs for MVP2 synapses
#Check how many non-MVP2 synapses are distal to each one
neuron_data <-catmaid_get_connectors_between(post_skids = skid)
neuron_data_dup <- neuron_data[!duplicated(neuron_data$post_node_id), ]
n.inputs <- neuron_data[neuron_data$pre_skid %in% input.skid,]
length(neuron_data_dup[neuron_data_dup$pre_skid %in% input.skid,]$post_node_id) #check none of the MVP2 inputs have been removed in the duplicated df
n_post_nodes <- n.inputs$post_node_id

#generate tree node IDs distal to each post_node_id of MVP2 synapse
index = lapply(n_post_nodes, function(x) match(x, n$d$PointNo))
library(elmr)
neuron.distal = lapply(index, function(x) distal_to(n, x))

#match these neuron.distal nodes to post_node_id of MVP2 and non-MVP2 synapses.
index.neurons <- lapply(c(1:40), function(x) match(n_post_nodes[[x]], n$d$PointNo))
distal.neurons.all <- lapply(index.neurons, function(x) distal_to(n, x))
#remove distal.neurons with only 1 value
indices <- match(c(115, 11847, 116, 117, 14142, 13087, 121, 13080), distal.neurons.all)
distal.neurons <- distal.neurons.all[-indices]
subset.neurons <- lapply(c(1:32), function(x) subset(n, distal.neurons[[x]]))
subset.count <- lapply(c(1:32), function(x) length(neuron_data_dup[neuron_data_dup$post_node_id  %in% subset.neurons[[x]]$d$PointNo,]$post_node_id))
subset.count.MVP2 <- lapply(c(1:32), function(x) length(n.inputs[n.inputs$post_node_id  %in% subset.neurons[[x]]$d$PointNo,]$post_node_id))

df <- do.call(rbind.data.frame, Map('c', subset.count, subset.count.MVP2))
df[nrow(df) + 1,] = as.numeric(1) #run 8 times because I haven't figured out how to do this another way yet
names(df)[1]<-paste("Non-MVP2 Inputs")
names(df)[2]<-paste("MVP2 Inputs")
df_final <-df-1