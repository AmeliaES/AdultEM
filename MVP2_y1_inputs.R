#Create figure of neuron with split axon and dendrite
skid = 2333007
cut_point = 4450405
MVP2 = read.neuron.catmaid(skid)
index = match(cut_point, MVP2$d$PointNo)
MVP2.dendrites = subset(MVP2, distal_to(MVP2, index))

plot3d(MVP2.dendrites, col = 'red', lwd = 3, WithConnectors = FALSE, WithNodes = FALSE, alpha = 0.5)
plot3d(MVP2, col = 'deepskyblue', lwd = 3, soma = TRUE, WithConnectors = FALSE, alpha = 0.5)

plot3d(FAFB13NP.surf, "MB_VL_L",col = "grey", alpha = 0.2)
plot3d(FAFB13NP.surf, "MB_VL_R",col = "grey", alpha = 0.2)
plot3d(FAFB13NP.surf, "MB_PED_L",col = "grey", alpha = 0.2)
plot3d(FAFB13NP.surf, "MB_ML_R",col = "grey", alpha = 0.2)
plot3d(FAFB13NP.surf, "MB_ML_L",col = "grey", alpha = 0.2)
plot3d(FAFB13NP.surf, "MB_PED_R",col = "grey", alpha = 0.2)
plot3d(FAFB13NP.surf, "MB_PED_L",col = "grey", alpha = 0.2)

rgl.snapshot("axon_dendrite.png")

### PLOT INPUT SYNAPSES TO MVP2 ####

nopen3d()
plot3d(MVP2.dendrites, col="black", WithConnectors = FALSE, WithNodes = FALSE, alpha = 0.3)  
plot3d(MVP2, col="black", WithConnectors = FALSE, WithNodes = FALSE, alpha = 0.3) 
#Or plot MVP2 using strahler ordering (see Strahler_order_plot.R script)

#Input neurons to MVP2
VPM3 = read.neuron.catmaid(1329078)
VPM4 = read.neuron.catmaid(1191261)
y4y1y2 = read.neuron.catmaid(870683)
aut = MVP2#dodgerblue4
APL = read.neuron.catmaid(203840) #darkslategray1
KCy = read.neurons.catmaid("annotation:AJES_KCy$")#Use annotation AJES_KCy
KCaB = read.neurons.catmaid("annotation:AJES_KCaB$")
MP1 = read.neurons.catmaid("annotation:AJES_MP1$")
DPM = read.neurons.catmaid(1150965)

#Generate connector IDs, skids ect. upstream of neuron of interest
neuron_data<-catmaid_get_connectors_between(post_skids = 2333007)

#Now select out of the list of pre_skids for those only containing certain skids and plot points of input
VPM3.inputs <-neuron_data[neuron_data$pre_skid %in% 1329078,]
VPM4.inputs <-neuron_data[neuron_data$pre_skid %in% 1191261,]
y4y1y2.inputs <-neuron_data[neuron_data$pre_skid %in% 870683,]
APL.inputs <-neuron_data[neuron_data$pre_skid %in% 203840,]
KCy.inputs <- nlapply(KCy, function(x) neuron_data[neuron_data$pre_skid %in% x,])
aut.inputs <-neuron_data[neuron_data$pre_skid %in% 2333007,]
KCaB.inputs <- nlapply(KCaB, function(x) neuron_data[neuron_data$pre_skid %in% x,])
MP1.inputs <- nlapply(MP1, function(x) neuron_data[neuron_data$pre_skid %in% x,])
DPM.inputs <- neuron_data[neuron_data$pre_skid %in% 1150965,]

#Plots inputs to MVP2
points3d(VPM3.inputs[,c('connector_x','connector_y','connector_z')], col="firebrick3", size=15)
points3d(VPM4.inputs[,c('connector_x','connector_y','connector_z')], col="firebrick3", size=15)
points3d(y4y1y2.inputs[,c('connector_x','connector_y','connector_z')], col="darkorange1", size=15)
points3d(APL.inputs[,c('connector_x','connector_y','connector_z')], col="cyan3", size=15)
KCy.points <- lapply(1:length(KCy.inputs), function(x) points3d(KCy.inputs[[x]][,c('connector_x','connector_y','connector_z')], col="gold1", size=15, alpha = 0.6))
points3d(aut.inputs[,c('connector_x','connector_y','connector_z')], col="deepskyblue", size=15)
KCaB.points <- lapply(1:length(KCaB.inputs), function(x) points3d(KCaB.inputs[[x]][,c('connector_x','connector_y','connector_z')], col="darkolivegreen", size=15, alpha = 0.6))
MP1.points <- lapply(1:length(MP1.inputs), function(x) points3d(MP1.inputs[[x]][,c('connector_x','connector_ys','connector_z')], col="darkorchid2", size=15))
points3d(DPM.inputs[,c('connector_x','connector_y','connector_z')], col="green", size=15)

snapshot3d("DPM.MVP2.inputs.png")
pop3d()
clear3d()
