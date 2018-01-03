#The number of synapses plotted (on a log scale) as a function from the root of MBON's dendrites,
#the point where the dendrites become a single axonal fiber. (Takemura et al. 2017)

skid = 4291899 #neuron B
input.skid = 2333007 #Neuron A
n = read.neuron.catmaid(skid)
#Choose a reference node to calculate distance from (below node is point on main neurite just prior to the dendritic field)
root = n$tags$soma
ng = as.ngraph(n, weights = T)
#Generate connector IDs, skids ect. upstream of n
neuron_data_MVP2 <-catmaid_get_connectors_between(post_skids = skid)
#As a test I chose VPM3 inputs to see if this works, but change the skid number to which ever neuron of input you're interested in
#This gives you details about all the synapses from neuron A to neuron B.
n.inputs_MVP2 <- neuron_data_MVP2[neuron_data_MVP2$pre_skid %in% input.skid,]
#To calculate distance do the following:
#Pull out post_node_ID 
n_post_nodes_MVP2 <- n.inputs_MVP2$post_node_id
#Calculate vertice indices for each treenode id
vids_MVP2 <- lapply(n_post_nodes_MVP2, function(x) match(x, n$d$PointNo))

#Get the vertice index for the reference node 
refvid_MVP2 <- match(root, n$d$PointNo)
#Gives distance (in nm) between post-synaptic nodes of n to M6 synapses and a reference tree node ID
library(igraph)
dist_MVP2 <- lapply(vids_MVP2, function(x) distances(ng, x, refvid_MVP2))
#Change distance to um and round to whole number
dist.r_MVP2 <- round(as.numeric(as.matrix(dist_MVP2))/1000)
#Put tree node ID and distance together in df
df_MVP2 <- data.frame(n_post_nodes_MVP2, as.matrix(dist.r_MVP2))


##### subset of KCs (n=12)
skid = 2109445 #neuron B
input.skid = 2333007 #Neuron A
n = read.neuron.catmaid(skid)
#Choose a reference node to calculate distance from (below node is point on main neurite just prior to the dendritic field)
root = n$tags$AJES_dendrites_distal
ng = as.ngraph(n, weights = T)
#Generate connector IDs, skids ect. upstream of n
neuron_data <-catmaid_get_connectors_between(post_skids = skid)
KCy <- read.neurons.catmaid("annotation:AJES_KCy_M6$")
KCy_skids <- lapply(c(1:length(KCy)), function(x) KCy[[x]]$skid)
n.inputs <- lapply(KCy_skids, function(x) neuron_data[neuron_data$pre_skid %in% x,])
#To calculate distance do the following:
#Pull out post_node_ID 
n_post_nodes_individ <- lapply(n.inputs, function(x) x$post_node_id)
n_post_nodes <- unlist(n_post_nodes_individ)
#Calculate vertice indices for each treenode id
vids <- lapply(n_post_nodes, function(x) match(x, n$d$PointNo))
#Get the vertice index for the reference node 
refvid <- match(root, n$d$PointNo)
#Gives distance (in nm) between post-synaptic nodes of n to M6 synapses and a reference tree node ID
library(igraph)
dist <- lapply(vids, function(x) distances(ng, x, refvid))
#Change distance to um and round to whole number
dist.r <- round(as.numeric(as.matrix(dist))/1000)
#Put tree node ID and distance together in df
df <- data.frame(n_post_nodes, as.matrix(dist.r))


#Define range
range <- c(min(df_MVP2$as.matrix.dist.r_MVP2.):max(df_MVP2$as.matrix.dist.r_MVP2.))

#KCys
#Count the frequency of each distance (ie. how many synapses occur at each distance point)
freq <- lapply(range, function(x) sum(df$as.matrix.dist.r.== x))
freq.mat <- as.matrix(freq)
freq.df <- as.data.frame(as.numeric(freq.mat))
names(freq.df)[1]<-paste("freq")
freq.df$distance <- range

#MVP2
#Count the frequency of each distance (ie. how many synapses occur at each distance point)
freq_MVP2 <- lapply(range, function(x) sum(df_MVP2$as.matrix.dist.r_MVP2 == x))
freq.mat_MVP2 <- as.matrix(freq_MVP2)
freq.df_MVP2 <- as.data.frame(as.numeric(freq.mat_MVP2))
names(freq.df_MVP2)[1]<-paste("freq")
freq.df_MVP2$distance <- range


#### Plot MVP2 -> M6 and non-MVP2 -> M6 on same bar plot.
barplot(freq.df$freq, names.arg = range, xlab = "Distance from root (um)", ylab = "Number of synapses", col = "grey")
par(new = T)
barplot(freq.df_MVP2$freq, names.arg = range, xlab = "Distance from root (um)", ylab = "Number of synapses", col = "red", axes = FALSE)



#First make an initial plot that includes the full range to define the axes, then plot data.

new_df <- cbind(freq.df$distance, freq.df$freq, freq.df_MVP2$freq)
new_df_2 <- as.data.frame(new_df)
names(new_df_2) <- paste(c("distance", "other", "MVP2"))


barplot(new_df_2$other, names.arg = range, xlab = "Distance from root (µm)", 
        ylab = "Number of synapses", col = "grey", ylim = c(0, 8.1))
par(new = T)
barplot(new_df_2$MVP2, col = "red", axes = FALSE, ylim = c(0, 8.1))
        #legend.text = c("MVP2 inputs", "other inputs", args.legend = col(as.matrix(c("grey", "red")))))
par(new = T)
legend(0, 8, legend = c("KCy inputs (n=12)", "MVP2 inputs"), fill = c("grey", "red"), cex = 0.75)


##Normalise
norm <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

dfNorm <- as.data.frame(lapply(new_df_2[2:3], norm))
dfNorm2 <- as.data.frame(cbind(new_df_2$distance, dfNorm$other, dfNorm$MVP2))
names(dfNorm2) <- paste(c("distance", "other", "MVP2"))


barplot(dfNorm2$other, names.arg = range, xlab = "Distance from root (µm)", ylab = "Synapse frequency (normalised)", col = "grey", ylim = c(0, 1))
par(new = T)
barplot(dfNorm2$MVP2, col = "red", axes = FALSE, ylim = c(0, 1))
#legend.text = c("MVP2 inputs", "other inputs", args.legend = col(as.matrix(c("grey", "red")))))
par(new = T)
#legend(col = c("grey", "red"))


#Averaging
fradf <- new_df_2
fradf$other_fraction <- as.numeric(lapply(fradf$other, function(x) x/1269))
fradf$MVP2_fraction <- as.numeric(lapply(fradf$MVP2, function(x) x/11))

barplot(fradf$other_fraction, names.arg = range, xlab = "Distance from root (µm)", ylab = "Synapse frequency (normalised)", col = "grey", ylim = c(0,0.2))
par(new = T)
barplot(fradf$MVP2_fraction, col = "red", axes = FALSE, ylim = c(0, 0.2))


#log
logdf <- new_df_2
logdf$other_log <- as.numeric(lapply(logdf$other, function(x) log(x)))
logdf$MVP2_log <- as.numeric(lapply(logdf$MVP2, function(x) log(x)))

barplot(logdf$other_log, names.arg = range, xlab = "Distance from root (µm)", ylab = "Synapse frequency (log scale)", col = "grey", ylim = c(0,4))
par(new = T)
barplot(logdf$MVP2_log, col = "red", axes = FALSE, ylim = c(0, 4))





