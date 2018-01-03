#calculate the number of non-MVP2, MVP2R and MVP2L synapses that occur distal to MVP2 synapses and the root
 
skid = 2109445
input.skid1 = 2333007
input.skid2 = 2120177


synapse_count <- function(skid, input.skid1, input.skid2){
n <- read.neuron.catmaid(skid)
#find node IDs for MVP2 synapses
#Check how many non-MVP2 synapses are distal to each one
neuron_data <-catmaid_get_connectors_between(post_skids = skid)
#neuron_data_dup <- neuron_data[!duplicated(neuron_data$post_node_id), ]
#test <- subset(neuron_data, !duplicated(neuron_data$post_node_id) && neuron_data$pre_skid == input.skid)
#check <<- length(neuron_data_dup[neuron_data_dup$pre_skid %in% input.skid,]$post_node_id) #check none of the MVP2 inputs have been removed in the duplicated df
n.inputs <- neuron_data[neuron_data$pre_skid %in% input.skid1,]
n_post_nodes <- n.inputs$post_node_id

#generate tree node IDs distal to each post_node_id of MVP2 synapse
index = lapply(n_post_nodes, function(x) match(x, n$d$PointNo))
library(elmr)
neuron.distal = lapply(index, function(x) distal_to(n, x))

#match these neuron.distal nodes to post_node_id of MVP2 and non-MVP2 synapses.
index.neurons <- lapply(c(1:length(n.inputs$post_node_id)), function(x) match(n_post_nodes[[x]], n$d$PointNo))
distal.neurons.all <- lapply(index.neurons, function(x) distal_to(n, x))

#remove distal.neurons with only 1 value
logic_neurons <- unlist(lapply(c(1:length(n.inputs$post_node_id)), function(x) length(distal.neurons.all[[x]]) > 1))
#Find indices for each FALSE value
indices <- which(logic_neurons == FALSE) #only gives first value

distal.neurons <- distal.neurons.all[-indices] #If this doesn't work/throws errors its because there are no FALSE values in "indices",
#therefore just commment out the above line of code like so: distal.neurons <- distal.neurons.all#[-indices]

subset.neurons <- lapply(c(1:length(distal.neurons)), function(x) subset(n, distal.neurons[[x]]))

#Check if any of the n_post_nodes appear in each subsetted neuron


subset.count <- lapply(c(1:length(distal.neurons)), function(x) length(neuron_data[neuron_data$post_node_id  %in% subset.neurons[[x]]$d$PointNo,]$post_node_id))
subset.count.MVP2 <- lapply(c(1:length(distal.neurons)), function(x) length(n.inputs[n.inputs$post_node_id  %in% subset.neurons[[x]]$d$PointNo,]$post_node_id))
n.inputs2 <- neuron_data[neuron_data$pre_skid %in% input.skid2,]
subset.count.MVP2L <- lapply(c(1:length(distal.neurons)), function(x) length(n.inputs2[n.inputs2$post_node_id  %in% subset.neurons[[x]]$d$PointNo,]$post_node_id))


df <- do.call(rbind.data.frame, Map('c', subset.count, subset.count.MVP2, subset.count.MVP2L))
names(df)[1]<-paste("Non-MVP2 Inputs")
names(df)[2]<-paste("Neuron 1 Inputs")
names(df)[3]<-paste("Neuron 2 Inputs")

#Only run this block if incides is not empty:
repeat_number <- length(which(logic_neurons == FALSE))
df[nrow(df) + repeat_number,] = as.numeric(0)
min<- length(distal.neurons)+1
max <- length(n.inputs$post_node_id)
df[c(min:max),] <-matrix(c(0,0,0), ncol = 3)


df$`Non-MVP2 Inputs` <- df$`Non-MVP2 Inputs` - df$`Neuron 1 Inputs`
df$`Neuron 1 Inputs` <- df$`Neuron 1 Inputs`-1
df$`Non-MVP2 Inputs` <- df$`Non-MVP2 Inputs` - df$`Neuron 2 Inputs`
df[df < 0] <- 0
return(df)
}


