library(catmaid)

#Return the raw data for a CATMAID neuronal skeleton
#catmaid_get_compact_skeleton(skid, pid = 1L, conn = NULL,
#                             connectors = TRUE, tags = TRUE, raw = FALSE, ...)
#example using MVP2 neuron, skid=2333007
MVP2<-catmaid_get_compact_skeleton(2333007, pid = 1L, conn = NULL,
                             connectors = TRUE, tags = TRUE, raw = FALSE)
summary(MVP2)

#Get the review status of neurons from CATMAID
catmaid_get_review_status(2333007)

#Return the tree node table for a given neuron
tree_tab<-catmaid_get_treenode_table(2333007)
summary(tree_tab)

#Get the names of neurons from CATMAID by giving it the skid
name<-catmaid_get_neuronnames(2333007)
print(name)

#Return a connector table for a given neuron
connector_tab<-catmaid_get_connector_table(2333007)
connector_tab

#Return information about connectors joining sets of pre/postsynaptic skids
#catmaid_get_connectors_between()

#Return skeleton ids for pre/postsynaptic partners of a set of connector_ids
#catmaid_get_connectors(connector_ids, pid = 1, conn = NULL, raw = FALSE, ...)

# read.neuron.catmaid() function source code 
function (skid, pid = 1L, conn = NULL, ...) 
{
  #First generates the skeleton and connector information.
  res = catmaid_get_compact_skeleton(pid = pid, skid = skid, 
                                     conn = conn, ...)
  
  
 #Checks that the skid given actually exists, if there are no nodes. ie. the length of the node subset of res data frame is false
  #then the function will stop prossesing.
   if (!length(res$nodes)) 
    stop("no valid nodes for skid:", skid)
  
  
  swc = with(res$nodes, data.frame(PointNo = id, Label = 0, 
                                   X = x, Y = y, Z = z, W = radius * 2, Parent = parent_id))
  swc$Parent[is.na(swc$Parent)] = -1L
  sp = somapos.catmaidneuron(swc = swc, tags = res$tags)
  soma_id_in_neuron = if (nrow(sp) == 0) 
    NULL
  else sp$PointNo
  n = nat::as.neuron(swc, origin = soma_id_in_neuron, skid = skid)
  n[names(res[-1])] = res[-1]
  if (length(n$connectors) < 1) 
    n$connectors = NULL
  fields_to_include = c("url", "headers")
  n[fields_to_include] = attributes(res)[fields_to_include]
  class(n) = c("catmaidneuron", "neuron")
  n
}

