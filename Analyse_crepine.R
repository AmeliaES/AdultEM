#Fetch neurons downstream of MVP2 in crepine
cre.neurons <- read.neurons.catmaid("annotation:MVP2 CRE downstream")
summary(cre.neurons)

#Get neurons names
cre.neurons.df <- as.data.frame(cre.neurons)
names <- cre.neurons.df$name


#Plot neurons
plot3d(cre.neurons, soma = TRUE, WithConnectors = TRUE)

#Remove APL
APL <- cre.neurons[[3]]
APL$skid
cre.neurons[[3]] <- NULL
#Check
cre.neurons[[3]]$skid
length(cre.neurons)

plot3d(cre.neurons, soma = TRUE, WithConnectors = FALSE)
#Plot neuropil regions 
names(FAFB13NP.surf$Regions)
plot3d(FAFB13)
plot3d(FAFB13NP.surf, "CRE.R", col = "grey", alpha = 0.2)
plot3d(FAFB13NP.surf, "CRE.L", col = "grey", alpha = 0.2)
plot3d(FAFB13NP.surf, "SIP.R", col = "grey", alpha = 0.2)
plot3d(FAFB13NP.surf, "SIP.L", col = "grey", alpha = 0.2)

#Group into similar clusters for analysis
#Annotations: potential pmP-a, asp-j soma lineage, SIP-CRE MVP2 lineage
pmpa.neurons <- read.neurons.catmaid("annotation:potential pMP-a")
aspj.similar <- read.neurons.catmaid("annotation:asp-j soma lineage")
sip.cre <- read.neurons.catmaid("annotation:SIP-CRE MVP2 lineage")

plot3d(FAFB13)
plot3d(pmpa.neurons, soma = TRUE, col = "dodgerblue1")
plot3d(aspj.similar, soma = TRUE, col = "black")
plot3d(sip.cre, soma = TRUE, col = "darkolivegreen")

#Subset cre.neurons to remove pmpa, aspj, and sip.cre neurons

cre.neurons.df$index <- seq.int(nrow(cre.neurons.df))
cre.neurons.subset <- cre.neurons[c(-1, -3, -8, -11, -12, -15, -16, -18, -21, -4, -5, -14, -17, -19, -22, -26, -28, -29, -32, -33, -36, -39, -40)]
#Plot extra neurons
plot3d(cre.neurons.subset, soma = TRUE)

######################################################################
plot3d(FAFB13, alpha = 0.1)
#Read neurons into catmaid in their clusters
catm.pmpa <- read.neurons.catmaid(c(1651222, 1650913, 1651122, 1651253, 1651511, 1651545, 1671518))
plot3d(catm.pmpa, col = "blue", soma = TRUE)

catm.sipcre <- read.neurons.catmaid(c(1307939, 3020395, 1613298))
catm.sipcre.overlap <- read.neurons.catmaid(c(2122442, 3615116))
plot3d(catm.sipcre, col = "red", soma = TRUE)
plot3d(catm.sipcre.overlap, col = "chocolate3", soma = TRUE)

catm.aspj <- read.neurons.catmaid(c(1670774, 1670066, 1651416))
plot3d(catm.aspj, col = "chartreuse4", soma = TRUE)

catm.mbons <- read.neurons.catmaid(c(1385339, 1619887, 2120177, 3503851, 546632, 551975))
plot3d(catm.mbons, col = "darkorchid", soma = TRUE, alpha = 0.5)

catm.dans <- read.neurons.catmaid(c(2938438, 1646797, 2250960))
plot3d(catm.dans, col = "darkseagreen2", soma = TRUE)

catm.pair <- read.neurons.catmaid(c(1651412, 1671425))
plot3d(catm.pair, col = "darkturquoise", soma = TRUE)

catm.anon <- read.neurons.catmaid(c(1650987, 1650992, 2388659, 1670287, 1670516, 1671124, 1671627, 1672505, 2451082))
plot3d(catm.anon, col = "gray35", soma = TRUE)







