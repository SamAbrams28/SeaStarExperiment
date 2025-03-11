library(tidyverse)
all_sites <- read_csv("AllSites.csv") 
all_sites$Combined_density <- all_sites$Purple_per_10m + all_sites$Orange_per_10m
pre_sswd <- all_sites[all_sites$Period == "Pre-SSWD", ]

density_vs_mortality <- pre_sswd %>%
  group_by(Site) %>%
  summarize(k = round(mean(Combined_density), 2), last_pre_density = last(Combined_density))

# I calculated these mortality rates last year. 
density_vs_mortality$Mortality_rate <- c(87, 20, 83, 100, 100, 100, 91, 100, 92, 88, 85, 98)

write.csv(density_vs_mortality, file = "Density_Mortality_Comparison.csv", row.names = FALSE)

library(ggplot2)
ggplot(densities_table, aes(last_pre_density, Mortality_rate)) +
  geom_point() +
  theme_bw()

ggplot(densities_table, aes(k, Mortality_rate)) +
  geom_point() +
  theme_bw()
