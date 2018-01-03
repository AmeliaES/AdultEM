#Generate cable length between two treenode IDs of a neuron.
#First tree node ID taken from list of post-synaptic treenodes
#Reference tree node identified manually
library(catmaid)
library(igraph)

M6 = read.neuron.catmaid(2109445)
#Choose a reference node to calculate distance from (below node is point on main neurite just prior to the dendritic field)
M6node = 8143885
M6ng = as.ngraph(M6, weights = T)

#Generate connector IDs, skids ect. upstream of M6
neuron_data_M6<-catmaid_get_connectors_between(post_skids = 2109445)
#Now select synapses only from MVP2
MVP2.M6.inputs <-neuron_data_M6[neuron_data_M6$pre_skid %in% 2333007,]
#Pull out post_node_ID from MVP2 to M6 synapses (ie. M6 tree nodes)
M6pointnos <- MVP2.M6.inputs$post_node_id
#Calculate vertice indices for each treenode id
M6vids <- lapply(M6pointnos, function(x) match(x, M6$d$PointNo))
#Get the vertice index for the reference node 
refvid <- match(M6node, M6$d$PointNo)

#Gives distance (in nm) between post-synaptic nodes of MVP2 to M6 synapses and a reference tree node ID
dist <- lapply(M6vids, function(x) distances(M6ng, x, refvid))
#Put tree node ID and distance together in df
M6df <- data.frame(M6pointnos, as.matrix(dist))
#Change names of collumns
names(M6df)[1]<-paste("Post_node_ID")
names(M6df)[2]<-paste("Distance to reference node (nm)")
View(M6df)




######################################LEFT SIDE#####################################



M6L = read.neuron.catmaid(2699293)
#Choose a reference node to calculate distance from 
#(below node is point on main neurite just prior to the dendritic field,
#it's also tagged as "ref_node")
M6Lnode = M6L$tags$ref_node
M6Lng = as.ngraph(M6L, weights = T)

#Generate connector IDs, skids ect. upstream of M6L
neuron_data_M6L<-catmaid_get_connectors_between(post_skids = 2699293)
#Now select synapses only from MVP2L
MVP2.M6L.inputs <-neuron_data_M6L[neuron_data_M6L$pre_skid %in% 2120177,]
#Pull out post_node_ID from MVP2L to M6L synapses (ie. M6L tree nodes)
M6Lpointnos <- MVP2.M6L.inputs$post_node_id
#Calculate vertice indices for each treenode id
M6Lvids <- lapply(M6Lpointnos, function(x) match(x, M6L$d$PointNo))
#Get the vertice index for the reference node 
M6Lrefvid <- match(17189905, M6L$d$PointNo)

#Gives distance (in nm) between post-synaptic nodes of MVP2L to M6L synapses and a reference tree node ID
dist <- lapply(M6Lvids, function(x) distances(M6Lng, x, M6Lrefvid))
#Put tree node ID and distance together in df
M6Ldf <- data.frame(M6Lpointnos, as.matrix(dist))
#Change names of collumns
names(M6Ldf)[1]<-paste("Post_node_ID")
names(M6Ldf)[2]<-paste("Distance to reference node (nm)")
View(M6Ldf)
