# Step 1: Queue up all 12 sites into the R sessioin
setwd("/Users/samanthaabrams/Documents/MarinCS\ 100B/SeaStarExperiment/Cleaned\ Dataframes")

almar <- read.csv("Cleaned\ Dataframes/AlmarAvenue.csv")
asilomar <- read.csv("Cleaned\ Dataframes/AsilomarStatePark.csv")
carmel <- read.csv("Cleaned\ Dataframes/CarmelPoint.csv")
coal_oil_pt <- read.csv("Cleaned\ Dataframes/CoalOilPoint.csv")
davenport <- read.csv("Cleaned\ Dataframes/DavenportLanding.csv")
fitzgerald <- read.csv("Cleaned\ Dataframes/FitzgeraldMarineReserve.csv")
nat_bridges <- read.csv("Cleaned\ Dataframes/NaturalBridges.csv")
pigeon_pt <- read.csv("Cleaned\ Dataframes/PigeonPoint.csv")
pillar_pt <- read.csv("Cleaned\ Dataframes/PillarPoint.csv")
pt_pinos <- read.csv("Cleaned\ Dataframes/PointPinos.csv")
san_simeon <- read.csv("Cleaned\ Dataframes/SanSimeon.csv")
wilder <- read.csv("Cleaned\ Dataframes/WilderRanch.csv")

  # fix coal oil point to account for the different socal sswd timeline
  coal_oil_pt[coal_oil_pt$Date == "11-19-2013", 9] <- "Pre-SSWD"

# Step 2: combine 12 data frames into one large one
library(dplyr)
all_sites <- bind_rows(almar, asilomar, carmel, coal_oil_pt, 
                       davenport, fitzgerald, nat_bridges, pigeon_pt, pillar_pt,
                       pt_pinos, san_simeon, wilder)

# Step 3: write the big data frame into a csv file so only 1 file has to be read in the future
setwd("/Users/samanthaabrams/Documents/MarinCS\ 100B/SeaStarExperiment")
write.csv(all_sites, file = "AllSites.csv", row.names = FALSE)



# Step 4: make a plot and customize it
library(ggplot2)
all_sites <- read.csv("AllSites.csv")

ggplot(all_sites, aes(Purple_Count, Orange_Count, color = Site, shape = Period)) +
  geom_point() +
  scale_shape_manual(values = c(1, 19))

# Density plot - removes outliers w/o pruning any data 
ggplot(all_sites, aes(Purple_per_10m, Orange_per_10m, color = Site, shape = Period)) +
  geom_point() +
  scale_shape_manual(values = c(1, 19)) +
  labs(x = "Purple Sea Stars per 10m^2",
       y = "Orange Sea Stars per 10m^2") +
  theme_bw()




# Zoomed-in plots
no_outlier <- data.frame(all_sites[-c(211:213),])
ggplot(no_outlier, aes(Purple_Count, Orange_Count, color = Site, shape = Period)) +
  geom_point() +
  scale_shape_manual(values = c(1, 19))

smallest_data <- data.frame(all_sites[all_sites$Purple.Count <= 30,])
ggplot(smallest_data, aes(Purple_Count, Orange_Count, color = Site, shape = Period)) +
  geom_point() +
  scale_shape_manual(values = c(1, 19))
