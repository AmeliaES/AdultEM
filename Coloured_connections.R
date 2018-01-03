#Code to show neuron of interest as a 3d plot, 
#with synapses in different colours corresponding to the neurons it is connected to.

#First plot neuron of interest, MVP2, in 3d.
MVP2=read.neurons.catmaid(2333007)
plot3d(MVP2, soma=2000, WithConnectors = T, col='Black')

#Plot neuron on brain volume
VFB_MVP2<-fetchn_fafb(2333007, mirror=FALSE)
plot3d(VFB_MVP2, soma=T, WithConnectors=T)
plot3d(FCWBNP.surf, alpha=0.2)
open3d()
plot3d(VFB_MVP2, soma=T, WithConnectors=T)
plot3d(FCWB)


VMP4=read.neuron.catmaid(1191261)
nlist=neuronlist(MVP2, VMP4)



#get neuron of interest 
neuron = read.neuron.catmaid(2333007)


#for upstream partners - incoming connections only
connectors.outgoing = connectors[connectors$prepost == 0,]
connectors.incoming = connectors[connectors$prepost == 1,]

#plot all incoming synapses on MVP2 neuron
plot3d(neuron)
points3d(connectors.incoming[,c('x','y','z')])


#Possible just to show the incoming connectors from 1 given neuron SKID?
#use the function
#catmaid_get_connectors()



upstream_connectors = neuron$connectors[0 %in% !neuron$connectors$prepost]


#Generate connector IDs for upstream of neuron of interest
conn=neuron$connectors
up_conn<- conn[conn$prepost!=0,]
up_conn2<-up_conn$connector_id

nrow(up_conn)
length(up_conn2)

#Pass this as an argument to generate skids
skid_info2<-catmaid_get_connectors(up_conn2)

#The above still did not work
#Try extracting rows only for when post synaptic skid is your neuron of interest

skid_info3<-skid_info[skid_info$post==2333007,]

#Try using the list of up_conn2, to generate a data frame using connectors df... 
#by which all rows with up_conn2 in connector_id colloum are accepted into a new df.

New_df<-connectors[connectors$connector_id %in% up_conn2,]


#Try again to generate skids of these connectors
new_skid<-New_df$connector_id
new_skid2<-catmaid_get_connectors(new_skid)
new_skid3<-catmaid_get_connectors_between(new_skid)
nrow(new_skid3)

#I think the following works and I was using the wrong function before
new_skid4<-catmaid_get_connectors_between(post_skids = 2333007)


nrow(new_skid4)
new_skid5<-catmaid_get_connectors_between(pre_skids = 2333007)
nrow(new_skid5)

plot3d(neuron)
points3d(connectors.incoming[,c('x','y','z')])
nopen3d()
plot3d(neuron)
points3d(new_skid4[,c('connector_x','connector_y','connector_z')])
#YIPPEE it worked!!
#Now we have a dataframe that contains all the connector points, the pre_skid ect
#Now select out of the list of pre_skids for those only containing certain skids, 
#lets start by showing the synapses of VMP4 with MVP2.

selected_pre_skids<-new_skid4[new_skid4$pre_skid %in% 1191261,]
nopen3d()
plot3d(neuron)
points3d(selected_pre_skids[,c('connector_x','connector_y','connector_z')])



#Change the colour of these points
nopen3d()
plot3d(neuron)
points3d(selected_pre_skids[,c('connector_x','connector_y','connector_z')], color="magenta")

#Next, we will look at how to have more than one skid by making it into a function.


