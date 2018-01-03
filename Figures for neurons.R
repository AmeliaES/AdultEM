#Makes a nice figure of a neuron, also plots neuropils and connectors.
n <- fetchn_fafb(2109445, mirror= FALSE)[[1]]
n2 <- fetchn_fafb(1085816, mirror= FALSE)[[1]]
n3 <- fetchn_fafb(2106411, mirror= FALSE)[[1]]
#Plot neuron of interest
plot3d(n, col = "black", soma = TRUE, WithConnectors = T)
plot3d(n2, col = "green", soma = TRUE)
plot3d(n3, col = "purple", soma = TRUE)

#Plot connectors
connectors <- n$connectors
connectors.outgoing = connectors[connectors$prepost == 0,]
connectors.incoming = connectors[connectors$prepost == 1,]
points3d(connectors.outgoing[,c('x','y','z')], col = "Red", size = 4, alpha = 0.5)
points3d(connectors.incoming[,c('x','y','z')], col ="Cyan", size = 4, alpha = 0.5)

connectors2 <- n2$connectors
connectors.outgoing2 = connectors2[connectors$prepost == 0,]
connectors.incoming2 = connectors2[connectors$prepost == 1,]
points3d(connectors.outgoing2[,c('x','y','z')], col = "Red", size = 4, alpha = 0.5)
points3d(connectors.incoming2[,c('x','y','z')], col ="Cyan", size = 4, alpha = 0.5)

connectors3 <- n3$connectors
connectors.outgoing3 = connectors3[connectors$prepost == 0,]
connectors.incoming3 = connectors3[connectors$prepost == 1,]
points3d(connectors.outgoing3[,c('x','y','z')], col = "Red", size = 4, alpha = 0.5)
points3d(connectors.incoming3[,c('x','y','z')], col ="Cyan", size = 4, alpha = 0.5)



#Plot neuropil regions
plot3d(FCWBNP.surf, "CRE.R", col = "grey", alpha = 0.2)
plot3d(FCWBNP.surf, "MB.VL.R", col = "orange", alpha = 0.2)
plot3d(FCWBNP.surf, "MB.PED.R", alpha = 0.2)
plot3d(FCWBNP.surf, "SIP.R", col = "grey", alpha = 0.2)

plot3d(FCWBNP.surf, "SIP.R",col = "grey", alpha = 0.2)
plot3d(FCWBNP.surf, "SIP.L",col = "grey", alpha = 0.2)
plot3d(FCWBNP.surf, "SLP.R",col = "magenta", alpha = 0.2)
plot3d(FCWBNP.surf, "SLP.L",col = "grey", alpha = 0.2)
plot3d(FCWBNP.surf, "SMP.R",col = "grey", alpha = 0.2)
plot3d(FCWBNP.surf, "SMP.L",col = "grey", alpha = 0.2)
plot3d(FCWBNP.surf, "LH.R",col = "grey", alpha = 0.2)
plot3d(FCWBNP.surf, "MB.VL.L",col = "grey", alpha = 0.2)
plot3d(FCWBNP.surf, "MB.ML.R",col = "grey", alpha = 0.2)
plot3d(FCWBNP.surf, "MB.ML.L",col = "grey", alpha = 0.2)
plot3d(FCWBNP.surf, "MB.PED.R",col = "grey", alpha = 0.2)
plot3d(FCWBNP.surf, col = "grey", alpha = 0.2)

pop3d()

names(FCWBNP.surf$Regions)


plot3d(JFRC2.surf)




pmpa=fetchn_fafb("name:^potential pMP-a$", mirror=F)






###

M6<- read.neuron.catmaid(2109445)
M6L <- read.neuron.catmaid(2699293)
M4<- read.neuron.catmaid(4291899)
M6all<- read.neurons.catmaid("annotation:AJES_M6_all")

nopen3d()
plot3d(M6, soma = TRUE, WithConnectors = TRUE, col = "black")

nopen3d()
plot3d(M6all, soma = TRUE, WithConnectors = TRUE, col = "black")

nopen3d()
plot3d(M6L, soma = TRUE, WithConnectors = TRUE, col = "black")
nopen3d()
plot3d(M4, soma = TRUE, WithConnectors = TRUE, col = "black")
#plot3d(FAFB14, alpha = 0.2)

clear3d()
