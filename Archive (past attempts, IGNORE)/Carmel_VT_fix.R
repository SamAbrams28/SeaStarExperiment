library(tidyverse)

# Function to convert wide format to table of mean abundances
summarizer <- function(csv){
  
  summary_table <- read_csv(csv) %>%
    pivot_longer(ends_with("Meters"),
               names_to = "Depth",
               values_to = "Abundance") %>%
    select(Date, Site, Abundance) %>%
    group_by(Date, Site) %>%
    summarize(Mean_abundance = round(mean(Abundance, na.rm = TRUE), 2),
            .groups = "drop") %>%
    mutate(Date = as.Date(Date, format = "%m-%d-%Y")) %>%
    arrange(Date)
  
  return(summary_table)
}

summarized_sansim <- summarizer("VT Fix/SanSim_Mussel.csv") 

# Table combiner function
combined_table <- function (site, species1, species2) {
  
  # File names
  species1_file <- str_glue("VT Fix/{site}_{species1}.csv")
  species2_file <- str_glue("VT Fix/{site}_{species2}.csv")
  
  # Run summarizer function on both csvs 
  species1_summary <- summarizer(species1_file)
  species2_summary <- summarizer(species2_file)
  
  # If one data frame is longer, add nas to the smaller one
  if (nrow(species1_summary) > nrow(species2_summary)) {
    nas <- rep(NA, nrow(species1_summary) - nrow(species2_summary))
    species2_abundance <- c(species2_summary$Mean_abundance, nas)
    species1_abundance <- species1_summary$Mean_abundance
    
    print("smaller species 2") # Just so I can check everything worked properly
    
  } else if (nrow(species1_summary) < nrow(species2_summary)) {
    nas <- rep(NA, nrow(species2_summary) - nrow(species1_summary))
    species1_abundance <- c(species1_summary$Mean_abundance, nas)
    species2_abundance <- species2_summary$Mean_abundance
    
    print("smaller species 1")
  }
  
  
  # New combined data frame
  combined <- data.frame(
    Site = species1_summary$Site,
    species1_Abundance = species1_abundance,
    species2_Abundance = species2_abundance,
    Date = species1_summary$Date
  )
  
  # Specify column names
  column2 <- str_glue("{species1}_Abundance")
  column3 <- str_glue("{species2}_Abundance")
  colnames(combined) <- c("Site", column2, column3, "Date")
  
  # Write combined file
  final_file_name <- str_glue("Cleaned Dataframes/{site}_VT.csv")
  write.csv(combined, file = final_file_name, row.names = FALSE)
}

combined_table("Carmel", "Mussel", "Acorn_Barnacle")
combined_table("Pinos", "Mussel", "Acorn_Barnacle")
combined_table("SanSim", "Mussel", "Acorn_Barnacle")
