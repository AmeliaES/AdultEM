#Calculate % input and output distal to a tree node
#Calculate cable length distal to a tree node
#library(elmr)
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
  
  y5Routputs_percent <- round(y5Rtotal_outputs_distal/nrow(outputs)*100, digits = 2)
  y5Rinputs_percent <- round(y5Rtotal_inputs_distal/nrow(inputs)*100, digits = 2)
  
  return(c(y5Rinputs_percent, y5Routputs_percent)) 
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
  
  y5Loutputs_percent <- round(y5Ltotal_outputs_distal/nrow(outputs)*100, digits = 2)
  y5Linputs_percent <- round(y5Ltotal_inputs_distal/nrow(inputs)*100, digits = 2)
  
  return(c(y5Linputs_percent, y5Loutputs_percent)) 
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
  
  
  
  SMPoutputs_percent <- round(SMPtotal_outputs_distal/nrow(outputs)*100, digits = 2)
  SMPinputs_percent <- round(SMPtotal_inputs_distal/nrow(inputs)*100, digits = 2)
  
  
  return(c(SMPinputs_percent, SMPoutputs_percent)) 
  
  
}




############Using function#########################################################################################################################################################

inout <- function(skid){
  y5R <- inout_y5R(skid)
  y5L <- inout_y5L(skid)
  SMP <- inout_SMP(skid)
  
    return(cbind(c(y5R[[1]], y5L[[1]], SMP[[1]]), c(y5R[[2]], y5L[[2]], SMP[[2]])))
}

n5 <- inout(3643424)
n2 <- inout(3025479)
n6 <- inout(5234721)
n7 <- inout(3026119)
n10 <- inout(5652208)

names <- c("n5.Inputs", "n5.Outputs", "n2.Inputs", "n2.Outputs", "n6.Inputs", "n6.Outputs", "n7.Inputs", "n7.Outputs", "n10.Inputs", "n10.Outputs")
spacing <- c(0.1, 0.1, 0.5, 0.1, 0.5, 0.1, 0.5, 0.1, 0.5, 0.1)
color <- c("skyblue1", "thistle3", "yellow3")

#Bar plot
barplot(cbind(n5, n2, n6, n7, n10), ylim = c(0, 100), xlim = c(0, 15), ylab = "Percentage (%)", axes = TRUE, names.arg = names, col = color, space = spacing, cex.names = 0.65, las = 1)
par(xpd=TRUE)
legend(13, 90, legend = c("y5R", "y5L", "SMP"), 
       fill =  color, cex = 1)
