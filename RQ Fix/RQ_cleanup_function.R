library(tidyverse)

combined_table <- function (site, species1, species2) {
  # File names
  species1_file <- str_glue("RQ Fix/{site}_{species1}.csv")
  original_species1 <- read.csv(species1_file)

  species2_file <- str_glue("RQ Fix/{site}_{species2}.csv")
  original_species2 <- read.csv(species2_file)

  # Check that data frames are equal length, stop the function if they're different
  if (nrow(original_species1) != nrow(original_species2)) {
    print("Error: Different number of observations of each species")
    stop()
  }

  # New combined data frame
  combined <- data.frame(
    Site = original_species1$Site,
    species1_Abundance = original_species1$Count.Mean,
    species2_Abundance = original_species2$Count.Mean,
    Date = original_species1$Date
  )

  # Specify column names
  column2 <- str_glue("{species1}_Abundance")
  column3 <- str_glue("{species2}_Abundance")
  colnames(combined) <- c("Site", column2, column3, "Date")

  
  # File name determination + write file into the cleaned table folder to keep things separate
  file_name <- paste(combined[1,1], "_RQ", ".csv") # Grab the site name
  no_spaces <- gsub(" ", "", file_name) # Remove spaces to make it a camel case file name
  pathed_file <- str_glue("Cleaned Dataframes/{no_spaces}")
  write.csv(combined, file = pathed_file, row.names = FALSE)
}

combined_table("Almar", "Mussel", "Acorn_Barnacle")
