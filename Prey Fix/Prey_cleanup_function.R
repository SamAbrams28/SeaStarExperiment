library(tidyverse)

# Function to convert wide format to table of mean abundances
summarizer <- function(csv){
  
  summary_table <- read_csv(csv) %>%
    pivot_longer(ends_with("Meters"),
                 names_to = "Depth",
                 values_to = "Abundance") %>%
    select(Date, Site, Abundance) %>%
    group_by(Date, Site) %>%
    summarize(Count.Mean = round(mean(Abundance, na.rm = TRUE), 2),
              .groups = "drop") %>%
    mutate(Date = as.Date(Date, format = "%m-%d-%Y")) %>%
    arrange(Date)
  
  return(summary_table)
}


# Table combiner function
combine_prey <- function (site, species1, species2, method) {
  
  # File names
  species1_file <- str_glue("Prey Fix/{site}_{species1}_{method}.csv")
  species2_file <- str_glue("Prey Fix/{site}_{species2}_{method}.csv")
  
  # Flip and summarize VT data. If RQ, the og data table is the "summary". 
  if (method == "VT"){
    # Run summarizer function on both csvs 
    species1_summary <- summarizer(species1_file)
    species2_summary <- summarizer(species2_file) 
  } else {
    species1_summary <- read.csv(species1_file)
    species2_summary <- read.csv(species2_file)
    }
   
  # If one data frame is longer, add nas to the smaller one
  if (nrow(species1_summary) > nrow(species2_summary)) {
    nas <- rep(NA, nrow(species1_summary) - nrow(species2_summary))
    species2_abundance <- c(species2_summary$Count.Mean, nas)
    species1_abundance <- species1_summary$Count.Mean
    
    # When making the combined df at the end, it needs to have the full list of
    # dates and sites
    all_observations <- species1_summary 
    
    print("smaller species 2") # Just so I can check everything worked properly
    
  } else if (nrow(species1_summary) < nrow(species2_summary)) {
    nas <- rep(NA, nrow(species2_summary) - nrow(species1_summary))
    species1_abundance <- c(species1_summary$Count.Mean, nas)
    species2_abundance <- species2_summary$Count.Mean
    
    all_observations <- species2_summary
    print("smaller species 1")
  } else {
    species1_abundance <- species1_summary$Count.Mean
    species2_abundance <- species2_summary$Count.Mean
    all_observations <- species1_summary
  }
  
  
  # New combined data frame
  combined <- data.frame(
    Site = all_observations$Site,
    species1_Abundance = species1_abundance,
    species2_Abundance = species2_abundance,
    Date = all_observations$Date
  )
  
  # Specify column names
  column2 <- str_glue("{species1}_Abundance")
  column3 <- str_glue("{species2}_Abundance")
  colnames(combined) <- c("Site", column2, column3, "Date")
  
  
  # File name determination + write file into the cleaned table folder to keep things separate
  no_spaces <- gsub(" ", "", combined[1,1]) # Remove spaces from site to make it a camel case file name
  pathed_file <- str_glue("Cleaned Dataframes/{no_spaces}_{method}.csv")
  write.csv(combined, file = pathed_file, row.names = FALSE)
  
  
  return(combined)
}

almar <- combine_prey("Almar", "Acorn_Barnacle", "Mussel", "RQ")
asilomar <- combine_prey("Asilomar", "Acorn_Barnacle", "Mussel", "VT")
carmel <- combine_prey("Carmel", "Acorn_Barnacle", "Mussel", "VT")
coaloil <- combine_prey("CoalOil", "Acorn_Barnacle", "Mussel", "VT")
davenport <- combine_prey("Davenport", "Acorn_Barnacle", "Mussel", "RQ")
fitzgerald <- combine_prey("Fitzgerald", "Acorn_Barnacle", "Mussel", "RQ")
natbridges <- combine_prey("Natbridges", "Acorn_Barnacle", "Mussel", "RQ")
pigeonpt <- combine_prey("PigeonPt", "Acorn_Barnacle", "Mussel", "RQ")
pillarpt <- combine_prey("PillarPt", "Acorn_Barnacle", "Mussel", "RQ")
pinos <- combine_prey("Pinos", "Acorn_Barnacle", "Mussel", "VT")
sansim <- combine_prey("SanSim", "Acorn_Barnacle", "Mussel", "VT")
wilder <- combine_prey("Wilder", "Acorn_Barnacle", "Mussel", "VT")

library(dplyr)
all_sites <- bind_rows(almar, asilomar, carmel, coaloil, 
                       davenport, fitzgerald, natbridges, pigeonpt, pillarpt,
                       pinos, sansim, wilder)
write_csv(all_sites, file = "AllSites_Prey.csv")
almar <- read.csv("Cleaned Dataframes/AlmarAvenue_RQ.csv")
almar$Date <- as.character(almar$Date)
asilomar$Date <- as.character(asilomar$Date)
carmel$Date <- as.character(carmel$Date)
coaloil$Date <- as.character(coaloil$Date)
davenport$Date <- as.character(davenport$Date)
fitzgerald$Date <- as.character(fitzgerald$Date)
natbridges$Date <- as.character(natbridges$Date)
pigeonpt$Date <- as.character(pigeonpt$Date)
pillarpt$Date <- as.character(pillarpt$Date)
pinos$Date <- as.character(pinos$Date)
sansim$Date <- as.character(sansim$Date)
wilder$Date <- as.character(wilder$Date)
