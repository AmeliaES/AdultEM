#Calculate % input and output distal to a tree node
#Calculate cable length distal to a tree node
library(elmr)
#skid <- 3025479
#skid <- 3643424
#Returns y5R inputs, outputs; y5L inputs, outputs; SMP inputs, outputs; as % of total inputs or outputs

inout_y5R <- function(skid){
n <- read.neuron.catmaid(skid)
#y5R
y5Rindex <- match(n$tags$AJES_y5Rdist, n$d$PointNo)
y5Rdist = distal_to(n, y5Rindex)
outputs <- catmaid_get_connectors_between(pre_skids = skid)
inputs <- catmaid_get_connectors_between(post_skids = skid)

if(length(y5Rindex) > 1){
  y5Rndp<-lapply(c(1:length(y5Rdist)), function(x) n$d[y5Rdist[[x]],])
  y5Roup <- lapply(c(1:length(y5Rdist)), function(x)outputs[outputs$pre_node_id %in% y5Rndp[[x]]$PointNo,])
  y5Rinp <- lapply(c(1:length(y5Rdist)), function(x)inputs[inputs$post_node_id %in% y5Rndp[[x]]$PointNo,])
  y5Rtotal_outputs_distal <- sum(unlist(lapply(c(1:length(y5Rdist)), function(x) nrow(y5Roup [[x]]))))
  y5Rtotal_inputs_distal <- sum(unlist(lapply(c(1:length(y5Rdist)), function(x) nrow(y5Rinp [[x]]))))
  output_names <- unlist(lapply(c(1:length(y5Rdist)), function(x)catmaid_get_neuronnames(y5Roup[[x]]$post_skid)))
  input_names <- unlist(lapply(c(1:length(y5Rdist)), function(x)catmaid_get_neuronnames(y5Rinp[[x]]$pre_skid)))
} else {
  y5Rndp<-n$d[y5Rdist,]
  y5Roup <- outputs[outputs$pre_node_id %in% y5Rndp$PointNo,]
  y5Rinp <- inputs[inputs$post_node_id %in% y5Rndp$PointNo,]
  y5Rtotal_outputs_distal <- sum(unlist(nrow(y5Roup)))
  y5Rtotal_inputs_distal <- sum(unlist(nrow(y5Rinp)))
  output_names <- unlist(catmaid_get_neuronnames(y5Roup$post_skid))
  input_names <- unlist(catmaid_get_neuronnames(y5Rinp$pre_skid))
}


###To find specific outputs/inputs

#Specific outputs
#M6, search for partial match to "y5B'2a"
M6ou <- sum(unlist(lapply(c(1:length(output_names)), function(x) length(grep(as.character("y5B'2a"), output_names[[x]])))))
#Other MBONs, search for "MBON" minus amount of M6
MBONou<- sum(unlist(lapply(c(1:length(output_names)), function(x) length(grep(as.character("MBON"), output_names[[x]])))))-M6ou
#"KC"
KCou <- sum(unlist(lapply(c(1:length(output_names)), function(x) length(grep(as.character("KC"), output_names[[x]])))))
#"DAN"
DANou <- sum(unlist(lapply(c(1:length(output_names)), function(x) length(grep(as.character("DAN"), output_names[[x]])))))
#other
otherou <- y5Rtotal_outputs_distal-(M6ou+MBONou+KCou+DANou)

#Specific inputs
#M6, search for partial match to "y5B'2a"
M6in <- sum(unlist(lapply(c(1:length(input_names)), function(x) length(grep(as.character("y5B'2a"), input_names[[x]])))))
#Other MBONs, search for "MBON" minus amount of M6
MBONin <- sum(unlist(lapply(c(1:length(input_names)), function(x) length(grep(as.character("MBON"), input_names[[x]])))))-M6in
#"KC"
KCin <- sum(unlist(lapply(c(1:length(input_names)), function(x) length(grep(as.character("KC"), input_names[[x]])))))
#"DAN"
DANin <- sum(unlist(lapply(c(1:length(input_names)), function(x) length(grep(as.character("DAN"), input_names[[x]])))))
#other, not traced ect
otherin <- y5Rtotal_inputs_distal-(M6in+MBONin+KCin+DANin)


#Calculate percentages

pM6in <- round(M6in/nrow(inputs)*100, digits = 2)
pMBONin <- round(MBONin/nrow(inputs)*100, digits = 2)
pKCin <- round(KCin/nrow(inputs)*100, digits = 2)
pDANin <- round(DANin/nrow(inputs)*100, digits = 2)
potherin <- round(otherin/nrow(inputs)*100, digits = 2)

pM6ou <- round(M6ou/nrow(outputs)*100, digits = 2)
pMBONou <- round(MBONou/nrow(outputs)*100, digits = 2)
pKCou <- round(KCou/nrow(outputs)*100, digits = 2)
pDANou <- round(DANou/nrow(outputs)*100, digits = 2)
potherou <- round(otherou/nrow(outputs)*100, digits = 2)

#y5Routputs_percent <- round(y5Rtotal_outputs_distal/nrow(outputs)*100, digits = 2)
#y5Rinputs_percent <- round(y5Rtotal_inputs_distal/nrow(inputs)*100, digits = 2)

return(c(pM6in, pMBONin, pKCin, pDANin, potherin, pM6ou, pMBONou, pKCou, pDANou, potherou)) 
}

#########################################################################y5L###############################################################################################################
inout_y5L <- function(skid){
  n <- read.neuron.catmaid(skid)
y5Lindex <- match(n$tags$AJES_y5Ldist, n$d$PointNo)
y5Ldist = distal_to(n, y5Lindex)

outputs <- catmaid_get_connectors_between(pre_skids = skid)
inputs <- catmaid_get_connectors_between(post_skids = skid)

  if(length(y5Lindex) > 1){
    y5Lndp<-lapply(c(1:length(y5Ldist)), function(x) n$d[y5Ldist[[x]],])
    y5Loup <- lapply(c(1:length(y5Ldist)), function(x)outputs[outputs$pre_node_id %in% y5Lndp[[x]]$PointNo,])
    y5Linp <- lapply(c(1:length(y5Ldist)), function(x)inputs[inputs$post_node_id %in% y5Lndp[[x]]$PointNo,])
    y5Ltotal_outputs_distal <- sum(unlist(lapply(c(1:length(y5Ldist)), function(x) nrow(y5Loup [[x]]))))
    y5Ltotal_inputs_distal <- sum(unlist(lapply(c(1:length(y5Ldist)), function(x) nrow(y5Linp [[x]]))))
    output_names <- unlist(lapply(c(1:length(y5Ldist)), function(x)catmaid_get_neuronnames(y5Loup[[x]]$post_skid)))
    input_names <- unlist(lapply(c(1:length(y5Ldist)), function(x)catmaid_get_neuronnames(y5Linp[[x]]$pre_skid)))
  } else {
    y5Lndp<-n$d[y5Ldist,]
    y5Loup <- outputs[outputs$pre_node_id %in% y5Lndp$PointNo,]
    y5Linp <- inputs[inputs$post_node_id %in% y5Lndp$PointNo,]
    y5Ltotal_outputs_distal <- sum(unlist(nrow(y5Loup)))
    y5Ltotal_inputs_distal <- sum(unlist(nrow(y5Linp)))
    output_names <- unlist(catmaid_get_neuronnames(y5Loup$post_skid))
    input_names <- unlist(catmaid_get_neuronnames(y5Linp$pre_skid))
  }


###To find specific outputs/inputs
#Specific outputs

#M6, search for partial match to "y5B'2a"
M6ou <- sum(unlist(lapply(c(1:length(output_names)), function(x) length(grep(as.character("y5B'2a"), output_names[[x]])))))
#Other MBONs, search for "MBON" minus amount of M6
MBONou<- sum(unlist(lapply(c(1:length(output_names)), function(x) length(grep(as.character("MBON"), output_names[[x]])))))-M6ou
#"KC"
KCou <- sum(unlist(lapply(c(1:length(output_names)), function(x) length(grep(as.character("KC"), output_names[[x]])))))
#"DAN"
DANou <- sum(unlist(lapply(c(1:length(output_names)), function(x) length(grep(as.character("DAN"), output_names[[x]])))))
#other
otherou <- y5Ltotal_outputs_distal-(M6ou+MBONou+KCou+DANou)

#Specific inputs

#M6, search for partial match to "y5B'2a"
M6in <- sum(unlist(lapply(c(1:length(input_names)), function(x) length(grep(as.character("y5B'2a"), input_names[[x]])))))
#Other MBONs, search for "MBON" minus amount of M6
MBONin <- sum(unlist(lapply(c(1:length(input_names)), function(x) length(grep(as.character("MBON"), input_names[[x]])))))-M6in
#"KC"
KCin <- sum(unlist(lapply(c(1:length(input_names)), function(x) length(grep(as.character("KC"), input_names[[x]])))))
#"DAN"
DANin <- sum(unlist(lapply(c(1:length(input_names)), function(x) length(grep(as.character("DAN"), input_names[[x]])))))
#other, not traced ect
otherin <- y5Ltotal_inputs_distal-(M6in+MBONin+KCin+DANin)


#Calculate percentages

pM6in <- round(M6in/nrow(inputs)*100, digits = 2)
pMBONin <- round(MBONin/nrow(inputs)*100, digits = 2)
pKCin <- round(KCin/nrow(inputs)*100, digits = 2)
pDANin <- round(DANin/nrow(inputs)*100, digits = 2)
potherin <- round(otherin/nrow(inputs)*100, digits = 2)

pM6ou <- round(M6ou/nrow(outputs)*100, digits = 2)
pMBONou <- round(MBONou/nrow(outputs)*100, digits = 2)
pKCou <- round(KCou/nrow(outputs)*100, digits = 2)
pDANou <- round(DANou/nrow(outputs)*100, digits = 2)
potherou <- round(otherou/nrow(outputs)*100, digits = 2)

#y5Routputs_percent <- round(y5Rtotal_outputs_distal/nrow(outputs)*100, digits = 2)
#y5Rinputs_percent <- round(y5Rtotal_inputs_distal/nrow(inputs)*100, digits = 2)

return(c(pM6in, pMBONin, pKCin, pDANin, potherin, pM6ou, pMBONou, pKCou, pDANou, potherou)) 
}


#####################################SMP################################################################################################################################################

inout_SMP <- function(skid){
  n <- read.neuron.catmaid(skid)
SMPindex <- match(n$tags$AJES_SMPproximal, n$d$PointNo)
SMProot <- match(n$tags$AJES_SMP_root, n$d$PointNo)
SMPdist = distal_to(n, node.idx = SMPindex, root.idx = SMProot)
#neuron.distal.points = n$d[SMPdist,]
#plot3d(n)
#points3d(neuron.distal.points[,c('X','Y','Z')], col = "deepskyblue")
outputs <- catmaid_get_connectors_between(pre_skids = skid)
inputs <- catmaid_get_connectors_between(post_skids = skid)

#if(length(SMPindex) > 1){
#  SMPndp<-lapply(c(1:length(SMPdist)), function(x) n$d[SMPdist[[x]],])
#  SMPoup <- lapply(c(1:length(SMPdist)), function(x)outputs[outputs$pre_node_id %in% SMPndp[[x]]$PointNo,])
#  SMPinp <- lapply(c(1:length(SMPdist)), function(x)inputs[inputs$post_node_id %in% SMPndp[[x]]$PointNo,])
#  SMPtotal_outputs_distal <- sum(unlist(lapply(c(1:length(SMPdist)), function(x) nrow(SMPoup [[x]]))))
#  SMPtotal_inputs_distal <- sum(unlist(lapply(c(1:length(SMPdist)), function(x) nrow(SMPinp [[x]]))))
#  output_names <- unlist(lapply(c(1:length(SMPdist)), function(x)catmaid_get_neuronnames(SMPoup[[x]]$post_skid)))
#  input_names <- unlist(lapply(c(1:length(SMPdist)), function(x)catmaid_get_neuronnames(SMPinp[[x]]$pre_skid)))
#} else {
  SMPndp<-n$d[SMPdist,]
  SMPoup <- outputs[outputs$pre_node_id %in% SMPndp$PointNo,]
  SMPinp <- inputs[inputs$post_node_id %in% SMPndp$PointNo,]
  SMPtotal_outputs_distal <- sum(unlist(nrow(SMPoup)))
  SMPtotal_inputs_distal <- sum(unlist(nrow(SMPinp)))
  
  if(length(SMPoup$post_skid > 0)){
  output_names <- unlist(catmaid_get_neuronnames(SMPoup$post_skid))
  #Specific outputs
  #M6, search for partial match to "y5B'2a"
  M6ou <- sum(unlist(lapply(c(1:length(output_names)), function(x) length(grep(as.character("y5B'2a"), output_names[[x]])))))
  #Other MBONs, search for "MBON" minus amount of M6
  MBONou<- sum(unlist(lapply(c(1:length(output_names)), function(x) length(grep(as.character("MBON"), output_names[[x]])))))-M6ou
  #"KC"
  KCou <- sum(unlist(lapply(c(1:length(output_names)), function(x) length(grep(as.character("KC"), output_names[[x]])))))
  #"DAN"
  DANou <- sum(unlist(lapply(c(1:length(output_names)), function(x) length(grep(as.character("DAN"), output_names[[x]])))))
  #other
  otherou <- SMPtotal_outputs_distal-(M6ou+MBONou+KCou+DANou)
  pM6ou <- round(M6ou/nrow(outputs)*100, digits = 2)
  pMBONou <- round(MBONou/nrow(outputs)*100, digits = 2)
  pKCou <- round(KCou/nrow(outputs)*100, digits = 2)
  pDANou <- round(DANou/nrow(outputs)*100, digits = 2)
  potherou <- round(otherou/nrow(outputs)*100, digits = 2)
    }else{
      pM6ou <- 0
    pMBONou <- 0
    pKCou <- 0
    pDANou <- 0
    potherou <- 0
  }
  
  input_names <- unlist(catmaid_get_neuronnames(SMPinp$pre_skid))
#}

#Specific inputs
#M6, search for partial match to "y5B'2a"
M6in <- sum(unlist(lapply(c(1:length(input_names)), function(x) length(grep(as.character("y5B'2a"), input_names[[x]])))))
#Other MBONs, search for "MBON" minus amount of M6
MBONin <- sum(unlist(lapply(c(1:length(input_names)), function(x) length(grep(as.character("MBON"), input_names[[x]])))))-M6in
#"KC"
KCin <- sum(unlist(lapply(c(1:length(input_names)), function(x) length(grep(as.character("KC"), input_names[[x]])))))
#"DAN"
DANin <- sum(unlist(lapply(c(1:length(input_names)), function(x) length(grep(as.character("DAN"), input_names[[x]])))))
#other, not traced ect
otherin <- SMPtotal_inputs_distal-(M6in+MBONin+KCin+DANin)


#Calculate percentages

pM6in <- round(M6in/nrow(inputs)*100, digits = 2)
pMBONin <- round(MBONin/nrow(inputs)*100, digits = 2)
pKCin <- round(KCin/nrow(inputs)*100, digits = 2)
pDANin <- round(DANin/nrow(inputs)*100, digits = 2)
potherin <- round(otherin/nrow(inputs)*100, digits = 2)



#y5Routputs_percent <- round(y5Rtotal_outputs_distal/nrow(outputs)*100, digits = 2)
#y5Rinputs_percent <- round(y5Rtotal_inputs_distal/nrow(inputs)*100, digits = 2)


return(c(pM6in, pMBONin, pKCin, pDANin, potherin, pM6ou, pMBONou, pKCou, pDANou, potherou)) 


}




############Using function#########################################################################################################################################################

inout <- function(skid){
  y5R <- inout_y5R(skid)
  y5L <- inout_y5L(skid)
  SMP <- inout_SMP(skid)
  
  y5RinM6 <- y5R[[1]]
  y5RinMBON <- y5R[[2]]
  y5RinKC <- y5R[[3]]
  y5RinDAN <- y5R[[4]]
  y5Rinother <- y5R[[5]]
  y5LinM6 <- y5L[[1]]
  y5LinMBON <- y5L[[2]]
  y5LinKC <- y5L[[3]]
  y5LinDAN <- y5L[[4]]
  y5Linother <- y5L[[5]]
  SMPinM6 <- SMP[[1]]
  SMPinMBON <- SMP[[2]]
  SMPinKC <- SMP[[3]]
  SMPinDAN <- SMP[[4]]
  SMPinother <- SMP[[5]]
  
  y5RouM6 <- y5R[[6]]
  y5RouMBON <- y5R[[7]]
  y5RouKC <- y5R[[8]]
  y5RouDAN<- y5R[[9]]
  y5Rouother<- y5R[[10]]
  y5LouM6 <- y5L[[6]]
  y5LouMBON <- y5L[[7]]
  y5LouKC <- y5L[[8]]
  y5LouDAN<- y5L[[9]]
  y5Louother<- y5L[[10]]
  SMPouM6 <- SMP[[6]]
  SMPouMBON <- SMP[[7]]
  SMPouKC <- SMP[[8]]
  SMPouDAN<- SMP[[9]]
  SMPouother<- SMP[[10]]

  return(cbind(c(y5RinM6, y5RinMBON, y5RinKC, y5RinDAN, y5Rinother, y5LinM6, y5LinMBON, y5LinKC, y5LinDAN, y5Linother, SMPinM6, SMPinMBON, SMPinKC, SMPinDAN, SMPinother),
               c(y5RouM6, y5RouMBON, y5RouKC, y5RouDAN, y5Rouother, y5LouM6, y5LouMBON, y5LouKC, y5LouDAN, y5Louother, SMPouM6, SMPouMBON, SMPouKC, SMPouDAN, SMPouother)))
}

n5 <- inout(3643424)
n2 <- inout(3025479)
n6 <- inout(5234721)
n7 <- inout(3026119)
n10 <- inout(5652208)

names <- c("n5.Inputs", "n5.Outputs", "n2.Inputs", "n2.Outputs", "n6.Inputs", "n6.Outputs", "n7.Inputs", "n7.Outputs", "n10.Inputs", "n10.Outputs")
spacing <- c(0.1, 0.1, 0.5, 0.1, 0.5, 0.1, 0.5, 0.1, 0.5, 0.1)
color <- c("darkslategray1", "darkseagreen1", "darkorchid1", "darkorange1", "firebrick1", "darkslategray3", "darkseagreen3", "darkorchid3", "darkorange3", "firebrick3",
           "darkslategray4", "darkseagreen4", "darkorchid4", "darkorange4", "firebrick4")

#Bar plot
barplot(cbind(n5, n2, n6, n7, n10), ylim = c(0, 100), xlim = c(0, 15), ylab = "Percentage (%)", axes = TRUE, names.arg = names, col = color, space = spacing, cex.names = 0.65, las = 1)
par(xpd=TRUE)
legend(13, 90, legend = c("y5R_M6", "y5R_MBON", "y5R_KC", "y5R_DAN", "y5R_unidentified", "y5L_M6", "y5L_MBON", "y5L_KC", "y5L_DAN", "y5L_unidentifed", 
                          "SMP_M6", "SMP_MBON", "SMP_KC", "SMP_DAN", "SMP_unidentified"), 
       fill =  color, cex = 0.75)

#####################


inout_2 <- function(skid){
  y5R <- inout_y5R(skid)
  y5L <- inout_y5L(skid)
  SMP <- inout_SMP(skid)
  
  y5RinM6 <- y5R[[1]]
  y5RinMBON <- y5R[[2]]
  y5RinKC <- y5R[[3]]
  y5RinDAN <- y5R[[4]]
  
  y5LinM6 <- y5L[[1]]
  y5LinMBON <- y5L[[2]]
  y5LinKC <- y5L[[3]]
  y5LinDAN <- y5L[[4]]
  
  SMPinM6 <- SMP[[1]]
  SMPinMBON <- SMP[[2]]
  SMPinKC <- SMP[[3]]
  SMPinDAN <- SMP[[4]]
  
  
  y5RouM6 <- y5R[[6]]
  y5RouMBON <- y5R[[7]]
  y5RouKC <- y5R[[8]]
  y5RouDAN<- y5R[[9]]
  
  y5LouM6 <- y5L[[6]]
  y5LouMBON <- y5L[[7]]
  y5LouKC <- y5L[[8]]
  y5LouDAN<- y5L[[9]]
  
  SMPouM6 <- SMP[[6]]
  SMPouMBON <- SMP[[7]]
  SMPouKC <- SMP[[8]]
  SMPouDAN<- SMP[[9]]
  
  
  return(cbind(c(y5RinM6, y5RinMBON, y5RinKC, y5RinDAN,  y5LinM6, y5LinMBON, y5LinKC, y5LinDAN, SMPinM6, SMPinMBON, SMPinKC, SMPinDAN),
               c(y5RouM6, y5RouMBON, y5RouKC, y5RouDAN, y5LouM6, y5LouMBON, y5LouKC, y5LouDAN, SMPouM6, SMPouMBON, SMPouKC, SMPouDAN)))
}

n.5 <- inout_2(3643424)
n.2 <- inout_2(3025479)
n.6 <- inout_2(5234721)
n.7 <- inout_2(3026119)
n.10 <- inout_2(5652208)



color2 <- c("darkslategray1", "darkseagreen1", "darkorchid1", "darkorange1", "darkslategray3", "darkseagreen3", "darkorchid3", "darkorange3", 
            "darkslategray4", "darkseagreen4", "darkorchid4", "darkorange4")

#Bar plot
barplot(cbind(n.5, n.2, n.6, n.7, n.10), ylim = c(0, 16), xlim = c(0, 13), ylab = "Percentage (%)", axes = TRUE, names.arg = names, col = color2, space = spacing, cex.names = 0.65, las = 1)
par(xpd=TRUE)
legend(11, 15, legend = c("y5R_M6", "y5R_MBON", "y5R_KC", "y5R_DAN", "y5L_M6", "y5L_MBON", "y5L_KC", "y5L_DAN","SMP_M6", "SMP_MBON", "SMP_KC", "SMP_DAN"), fill =  color2, cex = 0.75)





#Average pie chart
#pie(as.vector(c(y5Rtotal_inputs_distal, y5Rtotal_outputs_distal)))
