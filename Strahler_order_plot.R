#Plot neuron with different cable thickness corresponding to Strahler order

#Read neuron from CATMAID
skid = 2333007
MVP2 = read.neuron.catmaid(skid)

#Caluculate strahler order
stra.MVP2 = strahler_order(MVP2)
strahler = stra.MVP2$points
#Assign strahler order to neuron object
MVP2$d$strahler = strahler
max(strahler) #returns 8 (use below in for loop)

for (so in 1:8){
  plot3d(subset(MVP2, MVP2$d$strahler==so), lwd=so, soma = TRUE, col = "black", WithNodes = FALSE, alpha = 0.6)
}
#Plot the soma because this doesn't work in the above command
nopen3d()
plot3d(MVP2, lwd = 0, soma = TRUE, WithNodes = FALSE, col = "black", alpha = 0.6, WithLine = FALSE, WithAllPoints = FALSE, )

#Plot strahler order of a subsetted neuron (in this case dendrites)
cut_point = 4450405 #node ID of where the cut is made to subset neuron
index = match(cut_point, MVP2$d$PointNo)
MVP2.dendrites = subset(MVP2, distal_to(MVP2, index))

#Get strahler info for each point
MVP2.strahler.points = data.frame("MVP2_points" = MVP2$d$PointNo, "Strahler" = strahler)
den.points = data.frame("den_points" = MVP2.dendrites$d$PointNo)
MVP2.den.strahler = df[df$MVP2_points %in% den_points$den_points,]

#Plot subsetted neuron with different cable thickness corresponding to Strahler order
nopen3d()
for (so in 1:8){
  plot3d(subset(MVP2.dendrites, MVP2.den.strahler$Strahler==so), lwd=so, soma = TRUE, col = "black", WithNodes = FALSE, alpha = 0.6)
}

