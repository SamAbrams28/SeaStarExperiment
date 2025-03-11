sswd_onset <- as.Date("2013-09-01")

setwd("/Users/samanthaabrams/Documents/MarinCS\ 100B/SeaStarExperiment/Table\ Clean-Up/")

# Original data frame
almar_original <- read.csv('AlmarOriginal.csv')

# Make orange and purple their own columns, not rows
almar_orange <- almar_original[almar_original$Organism == "Ochre sea star (Orange) - Pisaster ochraceus",]
almar_purple <- almar_original[almar_original$Organism == "Ochre sea star (Purple/Brown) - Pisaster ochraceus", 11:12]
almar_combined <- data.frame(c(almar_orange, almar_purple))


# Add a column for the time period
almar_combined$Period <- "x"


# Set time period to be pre- or post-SSWD depending on the date
for (i in 1:length(almar_combined[,1])){
  if(as.Date(almar_combined[i,1], format = "%m-%d-%Y") <= sswd_onset){
    almar_combined[i, 16] <- "Pre-SSWD"
  } else {
    almar_combined[i, 16] <- "Post-SSWD"
  }
}


# Prune extraneous columns and name the remaining ones
almar_final <- almar_combined[, -c(1:2, 4:10, 13)]
colnames(almar_final) <- c("Site", "Orange Count", "Orange per 10 m^2", 
                           "Purple Count", "Purple per 10 m^2", "Period")

# File name determination + write file
trial <- paste(almar_final[1,1], ".csv")
no_spaces <- gsub(" ", "", trial)
setwd("/Users/samanthaabrams/Documents/MarinCS\ 100B/SeaStarExperiment/Cleaned\ Dataframes/")
write.csv(almar_final, file = no_spaces, row.names = FALSE)
