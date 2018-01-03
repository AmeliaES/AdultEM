#Colour axonletes of M4
library(elmr)


#Plots points in a colour
M4 <- read.neuron.catmaid(4291899)
cut_point <- M4$tags$AJES_dendrites_distal
index = match(cut_point, M4$d$PointNo)
neuron.distal = distal_to(M4, index)
neuron.distal.points = M4$d[neuron.distal,]

nopen3d()
plot3d(M4, col = "gray23")
points3d(neuron.distal.points[,c('X','Y','Z')], col = "deepskyblue")

#Subset neuron approach
subn <- subset(M4, neuron.distal)
plot3d(subn, col = "deepskyblue", WithNodes = FALSE) #MB small portions of neuron must be plotted first, 
                                                      #it will not overplot if you plot M4 first
plot3d(M4, col = "gray23")

#I suggest using lapply to make multiple cuts, and use the same tag for each node you want to look distal to.