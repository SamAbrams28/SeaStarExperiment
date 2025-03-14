library(tidyverse)
library(dplyr)
sswd_onset <- 2013

# Function for adding missing years
add_missing_years <- function (datatable, range) {
  all_years <- datatable # New data frame for all years
  
  # iterate through all rows of the data frame. Going to range means it iterates
  # through all of the rows in the data table + all of the rows that are added.
  for (i in 1:range){ 
    n = i+1
    if (all_years[n,1] != all_years[i,1] + 1) {
      all_years <- add_row(all_years, .after = i) 
      # new rows populate with NAs in all cells, add year and site 
      all_years[n,1] <- all_years[i,1] + 1 
      all_years[n,2] <- all_years[1,2] 
    }
  }
  return(all_years)
}



combine_species <- function (full_site, method) {
  
# ----------------- Prepare sea star data table first -----------------  
  
# 1. Add file path to csv name and read the csv: parameters → original
  star_file <- str_glue("Cleaned Dataframes/{full_site}.csv")
  original_stars <- read.csv(star_file) 
  
# 2. Add year column and assign based on date: original → original
  original_stars$Year <- 0 
  for (i in 1:nrow(original_stars)) {
    date <- as.Date(original_stars[i,8], format = "%m-%d-%Y")
    year <- format(date, "%Y") 
    
    # Group observations from Jan - Aug in with previous year
    if (as.numeric(format(date, "%m")) < 9) {
      year <- as.numeric(year) - 1
    } 
    original_stars[i,10] <- year
  }
  
# 3. Summarize data by year and prepare data table: original → by_year
  summarized_stars <- original_stars %>%
    group_by(Year, Site) %>%
    summarize(Purple_Count=round(mean(Purple_Count),2), 
              Purple_per_10m=round(mean(Purple_per_10m),2),
              Purple_relative_population=round(mean(Purple_relative_population),2),
              Orange_Count=round(mean(Orange_Count),2),
              Orange_per_10m=round(mean(Orange_per_10m),2),
              Orange_relative_population=round(mean(Orange_relative_population),2),
              .groups = "drop")
  
  # Make the summary table useful by making dates readable as numbers and table
  # readable as a data frame
  by_year_stars <- data.frame(summarized_stars) 
  by_year_stars$Year <- as.numeric(by_year_stars$Year)
  
# 4. Find the range of years in the final data frame: by_year...start, end, date_range
  start <- by_year_stars[1,1]
  end <- by_year_stars[nrow(by_year_stars),1]
  date_range <- end-start
    
# 5. Run missing years function on sea star data frame: by_year → stars_final
   stars_final <-add_missing_years(by_year_stars, date_range) 
  
  
# ----------------- Put prey data in the same format  ----------------- 
  
# 1. Add file path to csv name and read the csv: parameters → original
  prey_file <- str_glue("Cleaned Dataframes/{full_site}_{method}.csv")
  original_prey <- read.csv(prey_file) 
  
# 2. Add year column and assign based on date: original → original
  original_prey$Year <- 0 
  for (i in 1:nrow(original_prey)) {
    if (method == "RQ") {
      date <- as.Date(original_prey[i,4], format = "%m-%d-%Y")
    } else {
      date <- as.Date(original_prey[i,4], format = "%Y-%m-%d")
    }
    year <- format(date, "%Y") 
    
    # Group observations from Jan - Aug in with previous year
    if (as.numeric(format(date, "%m")) < 9) {
      year <- as.numeric(year) - 1
    } 
    original_prey[i,5] <- year
  }
  
# 3.1 Summarize data by year and prepare data table: original → in_range
  summarized_prey <- original_prey %>%
    group_by (Year, Site) %>%
    summarize (Mussel_Abundance = round(mean(Mussel_Abundance),2),
               Acorn_Barnacle_Abundance = round(mean(Acorn_Barnacle_Abundance),2),
               .groups = "drop")
  by_year_prey <- data.frame(summarized_prey) # Make table into an index-able dataframe 
  by_year_prey$Year <- as.numeric(by_year_prey$Year) # Make R recognize years as numbers
  
  # Get rid of all of the prey data outside the range for sea star data
  idx <- start <= by_year_prey$Year & by_year_prey$Year <= end
  in_range_prey <- by_year_prey[idx, ]
  
# 4. Find the starting range of years in the final prey data frame: in_range...start, end, range
  end_prey <- in_range_prey[nrow(in_range_prey),1]
  start_prey <- in_range_prey[1,1]
  range_prey <- end_prey-start_prey
  
# 5. Run missing years function on prey data frame: in_range → prey_final
  prey_final <- add_missing_years(in_range_prey, range_prey)

# 6. Add rows of NAs for missing years outside the prey range: final → final  
  # Add missing values at the beginning of the range
  if (start_prey != start) {
    site <- prey_final[1,2]
    start_difference <- start_prey - start
    for (i in 1:start_difference){
      prey_final <- add_row(prey_final, .before = 1)
      prey_final[1,1] <- prey_final[2,1] -1 
      prey_final[1,2] <- site
    }
  }
  
  # Add missing values at the end of the range
  if (end_prey != end) {
    site <- prey_final[1,2]
    end_difference <- end - end_prey
    for (i in 1:end_difference){
      prey_final <- add_row(prey_final)
      last_row <- nrow(prey_final)
      prey_final[last_row,1] <- prey_final[last_row-1,1] + 1
      prey_final[last_row,2] <- site
    }
  }  
  
# ----------------- Put the prey and sea star data together  -----------------   
  all_species <- bind_cols(stars_final, prey_final[,3:4])

# Add Period back in
  all_species$Period <- "x" # Add a column for the time period
  
  # Set time period to be pre- or post-SSWD depending on the date
  for (i in 1:nrow(all_species)){
    if(all_species[i,1] < sswd_onset){
      all_species[i, 11] <- "Pre-SSWD"
    } else {
      all_species[i, 11] <- "Post-SSWD"
    }
  }  
  
  # File name determination + write file into the cleaned table folder to keep things separate
  no_spaces <- gsub(" ", "", all_species[1,2]) # Remove spaces from site to make it a camel case file name
  pathed_file <- str_glue("Combined Dataframes/{no_spaces}_all_species.csv")
  write.csv(all_species, file = pathed_file, row.names = FALSE)
  
  return(all_species)
}


almar <- combine_species("AlmarAvenue", "RQ")
asilomar <- combine_species("AsilomarStatePark", "VT")
carmel <- combine_species("CarmelPoint", "VT")
coaloil <- combine_species("CoalOilPoint", "VT")
davenport <- combine_species("DavenportLanding", "RQ")
fitzgerald <- combine_species("FitzgeraldMarineReserve", "RQ")
natbridges <- combine_species("NaturalBridges", "RQ")
pigeon <- combine_species("PigeonPoint", "RQ")
pillar <- combine_species("PillarPoint", "RQ")
pinos <- combine_species("PointPinos", "VT")
sansim <- combine_species("SanSimeon", "VT")
wilder <- combine_species("WilderRanch", "VT")

library(dplyr)
all_sites <- bind_rows(almar, asilomar, carmel, coaloil, davenport, fitzgerald, 
                       natbridges, pigeon, pillar, pinos, sansim, wilder)
write_csv(all_sites, file = "AllSpecies_AllSites.csv")

library(ggplot2)
ggplot(almar, aes(Purple_relative_population, Acorn_Barnacle_Abundance, color = Period)) +
  geom_point() +
  theme_bw()

ggplot(almar, aes(Purple_relative_population, Orange_relative_population, color = Period, size = Year)) +
  geom_point() +
  scale_size(range = c(1,6)) + 
  theme_bw()
