#Takemura synapse locaion data
#Go to Takemura et al. 2017 paper and download zip file of additional infomation.
library(jsonlite)
synapse_location<-fromJSON("synapse.JSON")
annotation_body <-fromJSON("annotations-body.JSON")
a1 <- fromJSON("bool-lobe-1.json")

ab <-annotation_body$data
annotation_body$data$name

?fromJSON
an <- as.data.frame(annotation_body)
sy <- as.data.frame(synapse_location)
