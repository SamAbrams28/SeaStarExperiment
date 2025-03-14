sswd_onset <- as.Date("2013-09-01")

setwd("/Users/samanthaabrams/Documents/MarinCS\ 100B/SeaStarExperiment/Table\ Clean-Up/")

# Original data frame
almar_original <- read.csv('AlmarOriginal.csv')

# Make orange and purple their own columns, not rows
almar_purple <- almar_original[almar_original$Organism == "Ochre sea star (Purple/Brown) - Pisaster ochraceus", 11:12]
almar_orange <- almar_original[almar_original$Organism == "Ochre sea star (Orange) - Pisaster ochraceus",11:12]
almar_dates <- almar_original[almar_original$Organism == "Ochre sea star (Orange) - Pisaster ochraceus",c(1,3)]
almar_combined <- data.frame(c(almar_dates, almar_purple, almar_orange))

# Add a column for the time period
almar_combined$Period <- "x"


# Set time period to be pre- or post-SSWD depending on the date
for (i in 1:length(almar_combined[,1])){
  if(as.Date(almar_combined[i,1], format = "%m-%d-%Y") <= sswd_onset){
    almar_combined[i, 7] <- "Pre-SSWD"
  } else {
    almar_combined[i, 7] <- "Post-SSWD"
  }
}


# Relative purple density 
purple_k <- mean(almar_combined[almar_combined$Period == "Pre-SSWD", 4])
almar_combined$Purple_relative_population <- 0
almar_combined[,8] <- round(almar_combined[,4]/purple_k, 2)

# Relative orange density
orange_k <- mean(almar_combined[almar_combined$Period == "Pre-SSWD", 6])
almar_combined$Orange_relative_population <- 0
almar_combined[,9] <- round(almar_combined[,6]/orange_k, 2)



# Rearrange and name columns
almar_final <- almar_combined[, c(2:4, 8, 5:6, 9, 1, 7)]
colnames(almar_final) <- c("Site", "Purple Count", "Purple per 10 m^2",
                           "Orange Count", "Orange per 10 m^2", "Period")

# File name determination + write file
trial <- paste(almar_final[1,1], ".csv")
no_spaces <- gsub(" ", "", trial)
setwd("/Users/samanthaabrams/Documents/MarinCS\ 100B/SeaStarExperiment/Cleaned\ Dataframes/")
write.csv(almar_final, file = no_spaces, row.names = FALSE)
