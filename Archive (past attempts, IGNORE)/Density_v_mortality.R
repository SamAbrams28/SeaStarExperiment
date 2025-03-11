
data_frame <- data.frame(
  Site = c("Almar Avenue", "Asilomar State Park", "Carmel Point", 
           "Carpinteria State Beach", "Coal Oil Point", "Davenport Landing", 
           "Fitzgerald Marine Reserve", "Natural Bridges", "Pigeon Point",
           "Pillar Point", "Point Pinos", "San Simeon", "Wilder Ranch"),
  Last_pre_density = last_densities,
 # Density_k = , 
  Mortality_rate = c(87, 20, 83, -14, 100, 100, 100, 91, 100, 92, 88, 85, 98) 
)


library(tidyverse)
davenport <- read.csv("Cleaned Dataframes/DavenportLanding.csv") 
select(davenport, last_col(vars = "Pre-SSWD"))





data_frame_2 <- data_frame[-c(2,4),]


before_density <- function (csv) {
  data <- read.csv(csv)
  end.idx <- sum(data$Period == "Pre-SSWD")
  result <- data$Purple_per_10m[end.idx] + data$Orange_per_10m[end.idx]
  return(result)
}





library(ggplot2)
ggplot(data_frame_2, aes(Last_pre_density, Mortality_rate)) +
  geom_point() +
  theme_bw()

  
  


last_densities <- c(before_density("AlmarAvenue.csv"), 
before_density("AsilomarStatePark.csv"),
before_density("CarmelPoint.csv"),
socal_before_density("CarpinteriaStateBeach.csv"),
socal_before_density("CoalOilPoint.csv"),
before_density("DavenportLanding.csv"),
before_density("FitzgeraldMarineReserve.csv"),
before_density("NaturalBridges.csv"),
before_density("PigeonPoint.csv"),
before_density("PillarPoint.csv"),
before_density("PointPinos.csv"),
before_density("SanSimeon.csv"),
before_density("WilderRanch.csv")
)

