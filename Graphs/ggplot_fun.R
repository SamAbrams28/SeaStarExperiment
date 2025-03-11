library(ggplot2)
library(scales)
all_sites <- read.csv("AllSites.csv")
all_species <- read.csv("AllSpecies_AllSites.csv")
density_mortality <- read.csv("Density_Mortality_Comparison.csv")




ggplot(density_mortality, aes(last_pre_density, Mortality_rate)) +
  geom_point() +
  labs(x = "Last density recorded before SSWD onset (individuals per 10m^2)",
       y = "Ochre Sea Star Mortality Rate (%)") +
  theme_bw()

ggplot(density_mortality, aes(k, Mortality_rate)) +
  geom_point() +
  labs(x = "Pre-SSWD Average Population (individuals per 10m^2)",
       y = "Ochre Sea Star Mortality Rate (%)") +
  theme_bw()


ggplot(all_species, aes(x = Site, y = Mussel_Abundance, color = Site)) +
  geom_boxplot() +
  scale_x_discrete(labels = label_wrap(11)) +
  guides(color = "none") +
  theme_classic()


library(tidyverse)

all_species %>%
  select(ends_with("Abundance"))

long_format <- pivot_longer(all_species, c(ends_with("Abundance"), ends_with("Count")),
                           names_to = "Phenotype_Species", values_to = "Density_Abundance")



by_species <- long_format[,c(1,2,8,9)]
no_purple <- by_species[c(by_species$Phenotype_Species != "Purple_Count"),]

ggplot(no_purple, aes(x = Site, y = Density_Abundance, color = Phenotype_Species)) +
  geom_boxplot(aes(group = interaction(Phenotype_Species, Site))) +
  scale_color_discrete(name = "Species", labels = c("Acorn Barnacle Abundance", "Mussel Abundance", "Orange Ochre Sea Star Count")) +
  scale_x_discrete(labels = label_wrap(11)) +
  scale_y_continuous(limit = c(0,30)) +
  labs(y ="Count (total individuals)/Abundance (squares present per 1/4m plot)") +
  theme_classic() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.01, 0.99),
        legend.justification = c(0, 1))


4# Highlight the last observations before the beginning of the epizootic and the first ones after
all_sites$Key_point <- "No"

run_list <- rle(all_sites$Period)
run_lengths <- run_list$lengths
run_positions <- cumsum(run_lengths)
last_idx <- run_positions[run_list$values == "Pre-SSWD"]

all_sites[last_idx, 10] <- "Yes"
all_sites[last_idx +1 , 10] <- "Yes"



ggplot(all_sites, aes(Purple_per_10m, Orange_per_10m, shape = Period, 
                      color = Site, alpha = Key_point, size = Key_point)) +
  geom_point() +
  scale_shape_manual(values = c(1, 19)) +
  labs(x = "Purple Sea Stars per 10m^2",
       y = "Orange Sea Stars per 10m^2") +
  scale_color_hue(h = c(175, 360), l = 75, direction = 1) +
  scale_alpha_manual(values = c("Yes" = 1, "No" = 0.25)) + 
  scale_size_manual(values = c("Yes" = 4, "No" = 2)) +
  theme_bw()


library(stringr)

# Highlighted_relative_all
ggplot(all_sites, aes(Purple_relative_population, Orange_relative_population, 
         shape = Period, color = Site, alpha = Key_point, size = Key_point)) +
  geom_point() +
  scale_shape_manual(values = c(1, 19)) +
  labs(x = "Purple Sea Stars Relative Population",
           y = "Orange Sea Stars Relative Population") +
  scale_color_hue(h = c(175, 360), l = 75, direction = 1) +
  scale_alpha_manual(values = c("Yes" = 1, "No" = 0.25)) + 
  scale_size_manual(values = c("Yes" = 4, "No" = 2)) +
  theme(legend.text = element_text(size = 10)) +
  guides(labels = label_wrap(18) , alpha = "none", size = "none") 



ggplot(all_sites, aes(Purple_relative_population, Orange_relative_population, 
                     shape = Period, color = Site, alpha = Key_point, size = Key_point)) +
  geom_point() +
  scale_shape_manual(values = c(1, 19)) +
  labs(x = "Purple Sea Stars Relative Population",
       y = "Orange Sea Stars Relative Population") +
  scale_color_hue(h = c(175, 360), l = 75, direction = 1) +
  scale_alpha_manual(values = c("Yes" = 1, "No" = 0.25)) + 
  scale_size_manual(values = c("Yes" = 4, "No" = 2)) +
  theme_bw() +
  theme(legend.text = element_text(size = 10)) +
  guides(alpha = "none", size = "none") 


ggplot(all_sites, aes(Purple_per_10m, Orange_per_10m, color = Site, shape = Period)) +
  geom_point() +
  scale_shape_manual(values = c(1, 19)) +
  labs(x = "Purple Sea Stars per 10m^2",
       y = "Orange Sea Stars per 10m^2") +
  theme_bw()


library(dplyr)
reduced_sites <- bind_rows(read.csv("Cleaned Dataframes/AlmarAvenue.csv"), 
                           read.csv("Cleaned Dataframes/AsilomarStatePark.csv"),
                           read.csv("Cleaned Dataframes/CarmelPoint.csv"),
                           read.csv("Cleaned Dataframes/CoalOilPoint.csv"),
                           read.csv("Cleaned Dataframes/DavenportLanding.csv"))

# Brewer_paired_yearly.png and Brewer_paired_all.png
ggplot(all_species, aes(Purple_per_10m, Orange_per_10m, color = Site, shape = Period)) +
  geom_point() +
  scale_shape_manual(values = c(1, 19)) +
  labs(x = "Purple Sea Stars per 10m^2",
       y = "Orange Sea Stars per 10m^2") +
  scale_color_brewer(palette = "Paired") + 
  guides(labels = label_wrap(9)) # What the fuuuuuuuuuck

ggplot(all_sites, aes(Purple_per_10m, Orange_per_10m, color = Site, shape = Period)) +
  geom_point() +
  scale_shape_manual(values = c(1, 19)) +
  labs(x = "Purple Sea Stars per 10m^2",
       y = "Orange Sea Stars per 10m^2") +
  scale_color_brewer(palette = "Paired") +
  theme_bw() 


# Equal_axis_virdis.png - visualizes the difference between the population size ranges
ggplot(all_sites, aes(Purple_per_10m, Orange_per_10m, color = Site, shape = Period)) +
  geom_point() +
  scale_shape_manual(values = c(1, 19)) +
  labs(x = "Purple Sea Stars per 10m^2",
       y = "Orange Sea Stars per 10m^2") +
  scale_color_viridis_d(option = "D") + # C, D, F, and G are all nice. D is best for point
  scale_y_continuous(limit = c(0,4)) +
  scale_x_continuous(limit = c(0,4)) +
  theme_light()

ggplot(all_species, aes(Purple_per_10m, Orange_per_10m, color = Site, shape = Period)) +
  geom_point() +
  scale_shape_manual(values = c(1, 19)) +
  labs(x = "Purple Sea Stars per 10m^2",
       y = "Orange Sea Stars per 10m^2") +
  scale_color_viridis_d(option = "D") + # C, D, F, and G are all nice. D is best for point
  scale_y_continuous(limit = c(0,4)) +
  scale_x_continuous(limit = c(0,4)) +
  theme_light()



ggplot(all_sites, aes(Purple_Count, Orange_Count, color = Site, shape = Period)) +
  geom_point() +
  scale_shape_manual(values = c(1, 19)) +
  labs(x = "Total Purple Sea Stars",
       y = "Total Orange Sea Stars") +
  scale_color_viridis_d(option = "D") + # C, D, F, and G are all nice. D is best for point
  theme_linedraw()




# line_virdis
ggplot(all_species, aes(Purple_relative_population, Orange_relative_population, color = Site)) +
  geom_line(aes(linetype = Period, group = interaction(Period, Site)), key_glyph = "timeseries" ) +
  scale_shape_manual(values = c(1, 19)) +
  labs(x = "Purple Sea Stars Relative Population",
       y = "Orange Sea Stars Relative Population") +
  scale_color_viridis_d(option = "B") + 
  
  theme_light()







ggplot(all_sites, aes(Purple_relative_population, Orange_relative_population, color = Site, shape = Period)) +
  geom_point() +
  scale_shape_manual(values = c(1, 19)) +
  labs(x = "Purple Sea Star Relative Population",
       y = "Orange Sea Stars Relative Population") +
  scale_color_viridis_d(option = "D") + # C, D, F, and G are all nice. D is best for point
  theme_bw()

sites <- reduced_sites$Site
  
ggplot(reduced_sites, aes(Purple_per_10m, Orange_per_10m, color = Site, shape = Period)) +
  geom_line() +
  scale_shape_manual(values = c(1, 19)) +
  labs(x = "Purple Sea Stars per 10m^2",
       y = "Orange Sea Stars per 10m^2") +
  scale_color_hue(h = c(264, 340), l = 75, direction = 1) +
  theme_classic() 

ggplot(reduced_sites, aes(Purple_per_10m, Orange_per_10m, color = Site, shape = Period)) +
  geom_line() +
  scale_shape_manual(values = c(1, 19)) +
  labs(x = "Purple Sea Stars per 10m^2",
       y = "Orange Sea Stars per 10m^2") +
  scale_y_continuous(limit = c(0,6)) +
  scale_x_continuous(limit = c(0,6)) +
  theme_void()
