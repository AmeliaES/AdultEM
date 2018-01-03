#Count how many MVP2 and non-MVP2 synapses there are and distance between each non-MVP2 synapses and the root
library(elmr)
library(catmaid)

skid = 4291899
n = read.neuron.catmaid(skid)
root = n$tags$AJES_dendrites_distal


#1) Get all tree-nodes between each non-MVP2 input and AJES_root
# a) list of post-synaptic tree nodes that are non-MVP2 inputs

connector_table <- catmaid_get_connectors_between(post_skids = skid)
non_MVP2_inputs <- connector_table[!connector_table$pre_skid %in% 2333007,]
nodes <- non_MVP2_inputs$post_node_id

# b) find the list of tree nodes from each non-MVP2 synapse to the root
library(igraph)
ng=as.ngraph(n)
#Change root and treenodes to measure from into vertex indices
vid_root=match(root, n$d$PointNo)
vid_nodes= match(nodes, n$d$PointNo)

#Gets shortest path between each vid_node and vid_root. Contains treenode info. 
p= lapply(c(1:length(vid_nodes)), function(x) shortest_paths(ng, from=vid_root, to=vid_nodes[[x]], mode = 'all'))

#Gives list of strings of treenode_ids from each vid_node and vid_root.
treenode_p=lapply(p, function(x) n$d$PointNo[x$vpath[[1]]])

#2)
#How many MVP2_inputs (post_node_IDs) occur in each row.

MVP2_inputs <- connector_table[connector_table$pre_skid %in% 2333007,]$post_node_id
treenode_p_matrix <- as.matrix(treenode_p)

#Then I found out that passng two arguments to lapply is not possible. 
#test <- lapply(c(1:length(treenode_p)),c(1:length(MVP2_inputs)), function(x, y) sum((treenode_p[[x]]== MVP2_inputs[[y]]) == TRUE))

#func <- function(x, y){
#  unlist(sum((treenode_p[[x]]== MVP2_inputs[[y]]) == TRUE))
#}
#test2 <- lapply(c(1:length(treenode_p)), func, y = c(1:length(MVP2_inputs)))

#Stuck :(


indx <- sapply(treenode_p, length)
#indx <- lengths(lst) 
res <- as.data.frame(do.call(rbind,lapply(treenode_p, `length<-`,
                                          max(indx))))
colnames(res) <- names(treenode_p[[which.max(indx)]])






#debug
df <- n$d
df_new <- lapply(c(1:length(treenode_p)), function(x) subset(df, PointNo ==  treenode_p[[x]]))
pnt <- treenode_p[[1]]
df_new <- subset(df, PointNo ==  (treenode_p[[1]])
                 df_new2 <- do.call(rbind, df_new)
                 points3d(df_new2[,c('X','Y','Z')])
                 plot3d(n, soma = TRUE, col = "lightblue")
                 




MVP2_count <- lapply(c(1:length(MVP2_inputs)), function(x) subset(df_new2, PointNo == MVP2_inputs[[x]]))
MVP2_count2 <- do.call(rbind, MVP2_count)

























#b)
index = match(root, n$d$PointNo)
subn <- subset(n, distal_to(n, index))
#debug
plot3d(subn, WithNodes = FALSE, col = "black")
plot3d(n, col = "red", soma = TRUE)

#c)
#index2 <- match(nodes[[1]], subn$d$PointNo)
#subn2 <- subset(subn, distal_to(subn, index2), invert = TRUE)
#debug
plot3d(subn2, WithNodes = FALSE, col = "black")
plot3d(subn, WithNodes = FALSE, col = "red", soma = TRUE)

subn2stra <- prune_strahler(subn2, orderstoprune = 1:2)
plot3d(subn2stra, WithNodes = FALSE)




