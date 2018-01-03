#M6 <- read.neuron.catmaid(2109445)
#M6L<- read.neuron.catmaid(2109445)
#M4<- read.neuron.catmaid(4291899)

#Plot neuron and distal portion. Plot inputs from specific neuron. 
#Return how many inputs occur in each distal branch.

distal_plot <- function(skid, input_skid, distal_node){
  library(elmr)
  n = read.neuron.catmaid(skid = skid)
  index = match(n$tags$AJES_M6_middle, n$d$PointNo)
  distal = distal_to(n, index)
  distaln = subset(n, distal)
  plot3d(distaln, WithNodes = FALSE, col = "darkred", lwd = 3, alpha = 0.5)
  plot3d(n, col = "darkorange", lwd = 3, alpha = 0.2, soma = TRUE)
  
  neuron_data<-catmaid_get_connectors_between(post_skids = skid)
  inputs <-neuron_data[neuron_data$pre_skid %in% input_skid,]
  points3d(inputs[,c('connector_x','connector_y','connector_z')], col="darkolivegreen4", size=10)
  
  post_node <- inputs$post_node_id
  new_inputs <- distaln$d$PointNo %in% post_node
  return(sum(new_inputs == TRUE))
  
}

clear3d()
distal_plot(2109445, 2333007)


