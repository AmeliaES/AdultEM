#Makes a nice figure of a neuron, also plots neuropils and connectors.
library(elmr)
y4y5 <- fetchn_fafb(1619887, mirror= FALSE)[[1]]
y4y5 <- read.neuron.catmaid(1619887)
y4y1y2 <- fetchn_fafb(870683, mirror= FALSE)[[1]]
y3b1 <- fetchn_fafb(3037426, mirror= FALSE)[[1]]

pamy5 <- fetchn_fafb(3143746, mirror= FALSE)[[1]]
pamy4 <- fetchn_fafb(871796, mirror= FALSE)[[1]]
mbony3 <- fetchn_fafb(4394903, mirror= FALSE)[[1]]
mbona2 <- fetchn_fafb(1540941, mirror= FALSE)[[1]]
mbony2a1 <- fetchn_fafb("annotation:^AJES_y2a'1$", mirror= FALSE)[[1]]
pamb2p <- fetchn_fafb(3009846, mirror= FALSE)[[1]]

M4 <- fetchn_fafb(4291889, mirror= FALSE)[[1]]
M6 <- fetchn_fafb("annotation:^AJES_M6_all$", mirror= FALSE)[[1]]
M6L <- fetchn_fafb(2699293, mirror= FALSE)[[1]]


#Plot neuron of interest
plot3d(y4y5, col = "black", soma = TRUE, WithConnectors = T)
plot3d(y4y1y2, col = "green", soma = TRUE)
plot3d(y3b1, col = "purple", soma = TRUE)
plot3d(pamy5, col = "green", soma = TRUE)
plot3d(pamy4, col = "purple", soma = TRUE)
plot3d(mbony3, col = "green", soma = TRUE)
plot3d(mbona2, col = "purple", soma = TRUE)
plot3d(mbony2a1, col = "green", soma = TRUE)
plot3d(pamb2p, col = "purple", soma = TRUE)
plot3d(M4, col = "green", soma = TRUE)
plot3d(M6, col = "purple", soma = TRUE)
plot3d(M6L, col = "purple", soma = TRUE)


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

plot3d(FAFB14NP.surf)

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
