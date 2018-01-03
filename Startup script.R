#Run only if needed
if(!require("devtools")) install.packages("devtools")
devtools::source_gist("fdd1e5b6e009ff49e66be466a104fd92", filename = "install_flyconnectome_all.R")
options('nat.default.neuronlist'='dps')

#download dps, all in one go
dps<-read.neuronlistfh("http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/flycircuit/dpscanon.rds",localdir=getOption('flycircuit.datadir'))
remotesync(dps,download.missing = TRUE)

library(devtools)
library(elmr)
library(flycircuit)
library(doMC)
registerDoMC(7)

#surface model
FAFB13.surf=xform_brain(JFRC2013.surf, sample = JFRC2013, reference = FAFB13)

#ope downloaded stacks
open_stack<-function(x) system(paste("open", paste(shQuote(x), collapse = " ")))

#set download folder for vfbr stacks
options(vfbr.stack.downloads="/Users/aes/Downloads")

#open stack function: default Fiji
open_stack<-function(x) system(paste("open", paste(shQuote(x), collapse = " ")))