
unique(fc_neuron_type())

aspj <- fc_neuron_type(regex = "adult fruitless aSP-j neuron")
aspj

plot3d(names(aspj), soma = TRUE)


pmpa <- fc_neuron_type(regex = "pMP-a")
pmpa
plot3d(names(pmpa), soma = TRUE)


n <- fetchn_fafb(1670066, mirror= TRUE)[[1]]
plot3d(n, col = "black", soma = TRUE)
pop3d()

nopen3d()
plot3dfc("FruMARCM-M001918_seg001", soma= TRUE, col = "darkolivegreen3")
plot3dfc("FruMARCM-M002511_seg001", soma= TRUE, col = "goldenrod3")
plot3dfc("FruMARCM-F001125_seg001", soma= TRUE, col = "darkviolet")

pmpa
female1 <- pmpa[pmpa == "adult fruitless pMP-a (female) neuron"] #subsetting by the value
female2 <- pmpa["FruMARCM-F000991_seg003"] #subsetting by the name
#Subsetting a character vector, the element in pmpa, the name is the VFB neuron name, and the value is adult fruitless...ect

plot3d(names(female1), soma = TRUE)
fcwbnpsurf("CRE.L", alpha = 0.2, col = "grey")

clear3d()
nopen3d()
plot3d(names(female1[c(30:33, 37, 38)]), soma=TRUE)
pop3d()  
plot3d(names(female1[38]))
plot3d(n, col = "black", soma = TRUE, lwd = 4)
?plot3d

#Plot traced neurons on top
n1 <- fetchn_fafb(1651222, mirror= TRUE)
n2 <- fetchn_fafb(1650913, mirror= TRUE)
n3 <- fetchn_fafb(1651122, mirror= TRUE)
n4 <- fetchn_fafb(1651253, mirror= TRUE)
n5 <- fetchn_fafb(1651511, mirror= TRUE)
n6 <- fetchn_fafb(1651545, mirror= TRUE)
n7 <- fetchn_fafb(1671518, mirror= TRUE)

skids <- catmaid_skids(c(1651222, 1650913, 1651122, 1651253, 1651511, 1651545, 1671518))
neurons <- fetchn_fafb(skids, mirror= TRUE)
plot3d(neurons, col = "black", soma = TRUE, lwd = 3)
plot3d(FCWBNP.surf, col = "grey", alpha = 0.1)


names <- names(female1[c(30:33, 37, 38)])
fc_neuron(names)

pop3d()
plot3d(n7, soma = TRUE, col = "black", WithConnectors = TRUE)
