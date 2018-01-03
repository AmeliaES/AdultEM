#calculate the number of non-MVP2, MVP2R and MVP2L synapses that occur distal to MVP2 synapses and the root

skid = 2699293
input.skid2 = 2333007
input.skid1 = 2120177

#synapse_count_distal_tag <- function(skid, input.skid1, input.skid2){
  n <- read.neuron.catmaid(skid)
  #find node IDs for MVP2 synapses
  #Check how many non-MVP2 synapses are distal to each one
  neuron_data <-catmaid_get_connectors_between(post_skids = skid)
  n_post_nodes <- n$tags$AJES_MVP2_input
  
  #n.inputs <- neuron_data[neuron_data$pre_skid %in% input.skid1,]
  #n_post_nodes <- n.inputs$post_node_id
  
  #generate tree node IDs distal to each post_node_id of MVP2 synapse
  index = lapply(n_post_nodes, function(x) match(x, n$d$PointNo))
  library(elmr)
  neuron.distal = lapply(index, function(x) distal_to(n, x))
  
  #match these neuron.distal nodes to post_node_id of MVP2 and non-MVP2 synapses.
  index.neurons <- lapply(c(1:length(n_post_nodes)), function(x) match(n_post_nodes[[x]], n$d$PointNo))
  distal.neurons.all <- lapply(index.neurons, function(x) distal_to(n, x))
  
  #remove distal.neurons with only 1 value
  #logic_neurons <- unlist(lapply(c(1:length(n_post_nodes)), function(x) length(distal.neurons.all[[x]]) > 1))
  #Find indices for each FALSE value
  #indices <- which(logic_neurons == FALSE) 
  #true_indices <- which(logic_neurons == TRUE)
  
  #distal.neurons <- distal.neurons.all[-indices]
  
  subset.neurons <- lapply(c(1:length(distal.neurons.all)), function(x) subset(n, distal.neurons.all[[x]]))
  subset.count <- lapply(c(1:length(distal.neurons.all)), function(x) length(neuron_data[neuron_data$post_node_id  %in% subset.neurons[[x]]$d$PointNo,]$post_node_id))
  
  subset.count.MVP2 <- lapply(c(1:length(distal.neurons.all)), function(x) sum(n_post_nodes  %in% subset.neurons[[x]]$d$PointNo))
  
  #determine which fragments also have inputs from MVP2 and discount them
  logic2 <- lapply(c(1:length(subset.neurons)), function(x) n_post_nodes  %in% subset.neurons[[x]]$d$PointNo)
  indices2 <- lapply(c(1:length(subset.neurons)), function(x) which(logic2[[x]] == TRUE))
  df_compare <- data.frame(as.matrix(subset.count), as.matrix(subset.count.MVP2))
  
  #Total number of non-MVP2 inputs downstream of MVP2 input fragments, ignoring MVP2 inputs if they have already been counted.
  #Didn't have time to do this nicely with code yet so do this by counting df_compare inputs, disregarding indices that appear more than once in indices2
  
  df_manual <- data.frame(as.matrix(true_indices), as.matrix(subset.count), as.matrix(subset.count.MVP2), as.matrix(indices2))

  
M4 <- 132
M6L <- 146
M6 <- 1490

M4t <- 65
M6Lt <- 7
M6t <- 6

#M4 inputs = 3290
#M6 inputs = 1280
#M6L inputs = 2019

M4g <- M4/(3292-41)*100 
M6g <- M6/1490*100 
M6Lg <- M6L/(2177-15)*100 

data <- c(M4g, M6g, M6Lg)
barplot(data, ylim = c(0,100), ylab = "% of non-MVP2 inputs distal to MVP2 inputs", names.arg = c("M4", "M6", "M6(L)"))


M4gt <- M4t/(3292-41)*100 
M6gt <- M6t/1490*100 
M6Lgt <- M6t/(2177-15)*100 

datat <- c(M4gt, M6gt, M6Lgt)
barplot(datat, ylim = c(0,100), ylab = "% of non-MVP2 inputs distal to MVP2 inputs", names.arg = c("M4", "M6", "M6(L)"))
