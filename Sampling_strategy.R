#Analysis of the google drive sampling spreadsheet
#Analysis of sampling progress (discovery curves for new neurons / lineage / fragments; also piechart of proportions of fragments/skipped/traced to soma out of
#profiles sampled so far)
#open google doc and do file-download as and select .csv
data <- read.csv('/Users/aes/Downloads/CRE_288.csv')
#Cut off the rows that have not yet been sampled
cut_data <- data[data$X.3 == 'AJES',]
#set up summary data frame, the row number gives you the profiles sampled.
sum_up <- data.frame('Name' = rep(0,nrow(cut_data)), 'Lineage no' = rep(0,nrow(cut_data)), 'Cumulative new neurons' = rep(0,nrow(cut_data)), 'Cumulative fragments' = rep(0,nrow(cut_data)), 'Cumulative new lineage' = rep(0,nrow(cut_data)))
#returns modified sum_up dataframe with the cumulative new neuron, cumulative new lineage and cumulative fragment rows filled in
#Note: the fragment count does not currently account for hitting the same fragment multiple times, need to add in a step to check
#if name of fragment is the same as one seen before
Cumulative_totals <- function() {
  #counter for number of fragments
  frags <- 0
  #counter for number of new neurons
  new_neurs <- 0
  #counter for number of lineages
  new_lineage <- 0
  #loops through all rows in the df
  for (index in 1:nrow(cut_data)) {
    #checks if the name contains the word 'fragment'. If so puts name as 'FRAGMENT' and increases fragment counter.
    if (grepl('fragment', cut_data$X.12[index]) {
      frags = frags + 1
      sum_up$Name[index] <- 'FRAGMENT'
      sum_up$Lineage.no[index] <- '-'
      } else {
      #checks the name hasn't already been seen and is not '-'. Increments counter of new neurons
      if ((!(cut_data$X.9[index] %in% sum_up$Name)) & cut_data$X.9[index] != '-') {
        new_neurs = new_neurs + 1
        #Checks if lineage no is '-' and increments counter
        #if (cut_data$Neuron.lineage[index] == '-') {
          #new_lineage = new_lineage +1
        }
        # if has a lineage no, checks it has not already been seen. Increments counter of new lineage.
        #else if (!(cut_data$Neuron.lineage[index] %in% sum_up$Lineage.no)) {
          #new_lineage = new_lineage +1
        
      
      #Sets name in sum_up df to be that of the same row in cut_data; sets the lineage no to be that of the same row in cut_data
      sum_up$Name[index] <- as.character(cut_data$X.9[index])
      #sum_up$Lineage.no[index] <- as.character(cut_data$Neuron.lineage[index])
    }
    #sets the cumulative fragment/new neurons/new lineages to the counter values
    sum_up$Cumulative.fragments[index] <- frags
    sum_up$Cumulative.new.neurons[index] <- new_neurs
    #sum_up$Cumulative.new.lineage[index] <- new_lineage
    }
  invisible(sum_up)
}
#Calculate cumulative totals of new neurons (seen for first time in list) and the number of fragments
sum_up <- Cumulative_totals()
#Same df but with fragments and '-' rows excluded
fragments_excluded <- sum_up[sum_up$Name != 'FRAGMENT',]
fragments_excluded2 <- fragments_excluded[fragments_excluded$Name != '',]

#plot of cumulative new neurons against profiles sampled (includes fragments and skipped '-' rows), points that were fragments or skipped are marked in blue
plot(1:nrow(cut_data), sum_up$Cumulative.new.neurons, xlab = list('Profiles sampled'), ylab = list('Number of Neurons'), cex.lab=1.5, cex.axis=1.5, xlim=c(0,270),ylim=c(0,45), cex=0.7)
frag_rows <- as.integer(row.names(sum_up)[sum_up$Name == 'FRAGMENT' | sum_up$Name == '-'])
points(frag_rows, sum_up$Cumulative.new.neurons[frag_rows], col='blue', pch=20)
legend(170,30, pch=c(1,16), c('Unique neurons','Fragments/Skipped'), col=c('black','blue'), cex = 1.5)


#plot of cumulative new neurons against number of profiles sampled that were successfully traced to the soma
plot(1:nrow(fragments_excluded2), fragments_excluded2$Cumulative.new.neurons, xlab = 'Profiles sampled', ylab = 'Unique neurons', cex.lab = 1.5, cex.axis = 1.5)


#plot of cumulative new lineages against number of profiles sampled that were succesfully traced to the soma
plot(1:nrow(fragments_excluded), fragments_excluded$Cumulative.new.lineage, xlab = 'Profiles sampled', ylab = 'Unique Lineage', cex.lab = 1.5, cex.axis = 1.5)
#piechart of proportions of skipped, fragments and traced to soma profiles out of the ones checked so far
skipped <- sum(cut_data$Name == '-')
fragments <- sum(sum_up$Name == 'FRAGMENT')
traced <- nrow(fragments_excluded)
pie(c(skipped,fragments,traced), labels = c("Skipped", "Fragments", "Traced to soma"), cex=1.5,col = c('darkorange', 'darkturquoise', 'chartreuse3'))

test <- cut_data[grepl('fragment', cut_data$X.12)|grepl('fragment', cut_data$X.9) | (cut_data$X.9 == ''),]








Cumulative_totals <- function() {
  #counter for number of fragments
  frags <- 0
  #counter for number of new neurons
  new_neurs <- 0
  #counter for number of lineages
  new_lineage <- 0
  #loops through all rows in the df
  for (index in 1:nrow(cut_data)) {
    #checks if the name contains the word 'fragment'. If so puts name as 'FRAGMENT' and increases fragment counter.
    if (grepl('fragment', cut_data$X.12[index])){
      frags = frags + 1
      sum_up$Name[index] <- 'FRAGMENT'
      sum_up$Lineage.no[index] <- '-'
    } else {
      #checks the name hasn't already been seen and is not '-'. Increments counter of new neurons
      if ((!(cut_data$X.9[index] %in% sum_up$Name)) & cut_data$X.9[index] != '-') {
        new_neurs = new_neurs + 1
        #Checks if lineage no is '-' and increments counter
        # if (cut_data$Neuron.lineage[index] == '-') {
        # new_lineage = new_lineage +1
      }
      # if has a lineage no, checks it has not already been seen. Increments counter of new lineage.
      # else if (!(cut_data$Neuron.lineage[index] %in% sum_up$Lineage.no)) {
      #new_lineage = new_lineage +1
      
      
      #Sets name in sum_up df to be that of the same row in cut_data; sets the lineage no to be that of the same row in cut_data
      sum_up$Name[index] <- as.character(cut_data$X.9[index])
      #sum_up$Lineage.no[index] <- as.character(cut_data$Neuron.lineage[index])
    }
    #sets the cumulative fragment/new neurons/new lineages to the counter values
    sum_up$Cumulative.fragments[index] <- frags
    sum_up$Cumulative.new.neurons[index] <- new_neurs
    #sum_up$Cumulative.new.lineage[index] <- new_lineage
  }
  invisible(sum_up)
}