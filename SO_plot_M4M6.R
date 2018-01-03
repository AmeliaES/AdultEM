#Plot neuron with different cable thickness corresponding to Strahler order

plot_strahler_order = function(skid) {
  library(nat)
  library(catmaid)
  neuron = read.neuron.catmaid(skid = skid)
  stra.n = strahler_order(neuron)
  neuron$d$strahler = stra.n$points
    for (so in 1:max(stra.n$points)){
      plot3d(subset(neuron, neuron$d$strahler == so), lwd = so, soma = TRUE, col = "black", WithNodes = FALSE, alpha = 0.4)
      }
  plot3d(neuron, lwd = 0, soma = TRUE, WithNodes = FALSE, col = "black", alpha = 0.6, WithLine = FALSE, WithAllPoints = FALSE)
  
}

#Input neurons to M4 M6
MVP2R = read.neuron.catmaid(2333007)
MVP2L = read.neuron.catmaid(2120177)

#Generate connector IDs, skids ect. upstream of neuron of interest
neuron_data_M4<-catmaid_get_connectors_between(post_skids = 4291899)
neuron_data_M6R<-catmaid_get_connectors_between(post_skids = 2109445)
neuron_data_M6L<-catmaid_get_connectors_between(post_skids = 2699293)

#Now select out of the list of pre_skids for those only containing certain skids and plot points of input
MVP2R.M4.inputs <-neuron_data_M4[neuron_data_M4$pre_skid %in% 2333007,]
MVP2L.M4.inputs <-neuron_data_M4[neuron_data_M4$pre_skid %in% 2120177,]

MVP2R.M6L.inputs <-neuron_data_M6L[neuron_data_M6L$pre_skid %in% 2333007,]
MVP2L.M6L.inputs <-neuron_data_M6L[neuron_data_M6L$pre_skid %in% 2120177,]

MVP2R.M6R.inputs <-neuron_data_M6R[neuron_data_M6R$pre_skid %in% 2333007,]
MVP2L.M6R.inputs <-neuron_data_M6R[neuron_data_M6R$pre_skid %in% 2120177,]


#Plots inputs 

nopen3d()
M6R = plot_strahler_order(2109445)
points3d(MVP2R.M6R.inputs[,c('connector_x','connector_y','connector_z')], col="cyan", size=10)
points3d(MVP2L.M6R.inputs[,c('connector_x','connector_y','connector_z')], col="firebrick3", size=10)

nopen3d()
M6L = plot_strahler_order(2699293)
points3d(MVP2R.M6L.inputs[,c('connector_x','connector_y','connector_z')], col="cyan", size=10)
points3d(MVP2L.M6L.inputs[,c('connector_x','connector_y','connector_z')], col="firebrick3", size=10)



nopen3d()
M4 = plot_strahler_order(4291899)
points3d(MVP2R.M4.inputs[,c('connector_x','connector_y','connector_z')], col="cyan", size=10)
points3d(MVP2L.M4.inputs[,c('connector_x','connector_y','connector_z')], col="firebrick3", size=10)


























#Plot strahler order of a subsetted neuron (in this case dendrites)

plot_strahler_order = function(skid, cut_point) {
  library(nat)
  library(catmaid)
  library(elmr)
  neuron = read.neuron.catmaid(skid = skid)
  index = match(cut_point, neuron$d$PointNo)
  neuron.distal = subset(neuron, distal_to(neuron, index))
  stra.n = strahler_order(neuron)
  neuron$d$strahler = stra.n$points
  neuron.strahler.points = data.frame("Neuron_points" = neuron$d$PointNo, "Strahler" = stra.n$points)
  n.distal.points = data.frame("dist.points" = neuron.distal$d$PointNo)
  n.distal.stra = neuron.strahler.points[neuron.strahler.points$Neuron_points %in% n.distal.points,]
  nopen3d()
  for (so in 1:max(stra.n$points)){
    plot3d(subset(neuron.distal, n.distal.stra$strahler==so), lwd=so, soma = TRUE, col = "black", WithNodes = FALSE, alpha = 0.6)
  }
}
















M6 = read.neuron.catmaid(2109445)
cut_point = M6$tags$AJES_dendrites_distal













plot_strahler_order_dendrites = function(skid, cut_point) {
  library(nat)
  library(catmaid)
  library(elmr)
  neuron = read.neuron.catmaid(skid = skid)
index = match(cut_point, n$d$PointNo)
n.dendrites = subset(n, distal_to(n, index))

#Get strahler info for each point
stra.n = strahler_order(n)
strahler = stra.n$points
neuron$d$strahler = strahler
n.strahler.points = data.frame("n_points" = n$d$PointNo, "Strahler" = strahler)
den_points = data.frame("den_points" = n.dendrites$d$PointNo)
n.den.strahler = df[df$n_points %in% den_points$den_points,]

#Plot subsetted neuron with different cable thickness corresponding to Strahler order
nopen3d()
for (so in 1:max(stra.n$points)){
  plot3d(subset(n.dendrites, n.den.strahler$Strahler==so), lwd=so, soma = TRUE, col = "black", WithNodes = FALSE, alpha = 0.6)
}
}

