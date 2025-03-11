sswd_onset <- as.Date("2013-09-01")
library(tidyverse)

# Make a function
clean_table <- function (original){ 
  
  # Add file path to csv name and read the csv
  file_call <- str_glue("Table Clean-Up/{original}")
  original_dataframe <- read.csv(file_call)

  # Separate purple data, orange data, and the other needed info (date + site)
  purple <- original_dataframe[original_dataframe $Organism == "Ochre sea star (Purple/Brown) - Pisaster ochraceus", 11:12]
  orange <- original_dataframe[original_dataframe $Organism == "Ochre sea star (Orange) - Pisaster ochraceus",11:12]
  dates <- original_dataframe[original_dataframe $Organism == "Ochre sea star (Orange) - Pisaster ochraceus",c(1,3)]
 
  # Test that orange and purple are equal, stop the function if they're different
  if (length(purple[,1]) != length(orange[,1])) {
    print("Error: Different number of observations of each color")
    stop()
  }

  # Combine everything back into one data frame
  combined <- data.frame(c(dates, purple, orange))
  
  # Add a column for the time period
  combined$Period <- "x" 

  # Set time period to be pre- or post-SSWD depending on the date
  for (i in 1:length(combined[,1])){
    if(as.Date(combined[i,1], format = "%m-%d-%Y") <= sswd_onset){
      combined[i, 7] <- "Pre-SSWD"
    } else {
      combined[i, 7] <- "Post-SSWD"
    }
  }  

  # Relative purple density 
  purple_k <- mean(combined[combined$Period == "Pre-SSWD", 4])
  combined$Purple_relative_population <- 0
  combined[,8] <- round(combined[,4]/purple_k, 2)
  
  # Relative orange density
  orange_k <- mean(combined[combined$Period == "Pre-SSWD", 6])
  combined$Orange_relative_population <- 0
  combined[,9] <- round(combined[,6]/orange_k, 2)

  
  # Rearrange and name columns
  final <- combined[, c(2:4, 8, 5:6, 9, 1, 7)]
  colnames(final) <- c("Site", "Purple_Count", "Purple_per_10m", "Purple_relative_population",
                     "Orange_Count", "Orange_per_10m", "Orange_relative_population","Date", "Period")

  # File name determination + write file into the cleaned table folder to keep things separate
  trial <- paste(final[1,1], ".csv") # Grab the site name
  no_spaces <- gsub(" ", "", trial) # Remove spaces to make it a camel case file name
  pathed_file <- str_glue("Cleaned Dataframes/{no_spaces}")
  write.csv(final, file = pathed_file, row.names = FALSE)

} 

clean_table("AlmarOriginal.csv")
clean_table("AsilomarOriginal.csv")
clean_table("CarmelOriginal.csv")
clean_table("CoalOilOriginal.csv")
clean_table("DavenportOriginal.csv")
clean_table("FitzgeraldOriginal.csv")
clean_table("NaturalBridgesOriginal.csv")
clean_table("PigeonPtOriginal.csv")
clean_table("PillarPtOriginal.csv") # Produced the unequal orange and purple error
clean_table("PillarPtEven.csv")
clean_table("PtPinosOriginal.csv")
clean_table("SanSimeonOriginal.csv")
clean_table("WilderRanchOriginal.csv")

# Prune sampling event that only has observations for one color
pillar_original <- read.csv("PillarPtOriginal.csv")
pillar_dates <- pillar_original[,1]
table(pillar_dates)
error <- which(pillar_dates == "11-04-2014")
pillar_even <- pillar_original[-error,]
write.csv(pillar_even, file = "PillarPtEven.csv", row.names = FALSE)
