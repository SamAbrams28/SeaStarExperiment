library(ggplot2)
library(scales)
all_sites <- read.csv("AllSites.csv")
all_species <- read.csv("AllSpecies_AllSites.csv")
density_mortality <- read.csv("Density_Mortality_Comparison.csv")

# none of the wrap commands are working for the legend, so this fixes the
# problem of the uniquely lengthy site name
all_sites$Site <- gsub("Fitzgerald Marine Reserve", "Fitzgerald Marine \n Reserve", all_sites$Site)


# Mussel box plot 
ggplot(all_species, aes(x = Site, y = Mussel_Abundance, color = Site)) +
  geom_boxplot() +
  scale_x_discrete(labels = label_wrap(11)) +
  labs(y = "Mussel Abundance") +
  guides(color = "none") +
  theme_classic()



# ----- Scatter plots -----

# Most basic plot
ggplot(all_sites, aes(Purple_per_10m, Orange_per_10m, color = Site, shape = Period)) +
  geom_point() +
  scale_shape_manual(values = c(1, 19)) +
  labs(x = "Purple Sea Stars per 10m^2",
       y = "Orange Sea Stars per 10m^2") +
  theme_bw()


# ----- experimenting with colors and axis ------ 

# Brewer_paired_mussels.png and Brewer_paired_stars.png
ggplot(all_species, aes(Purple_per_10m, Mussel_Abundance, color = Site, shape = Period)) +
  geom_point() +
  scale_shape_manual(values = c(1, 19)) +
  labs(x = "Purple Sea Stars per 10m^2",
       y = "Mussel Abundance") +
  scale_color_brewer(palette = "Paired") + 
  theme_light()

ggplot(all_sites, aes(Purple_per_10m, Orange_per_10m, color = Site, shape = Period)) +
  geom_point() +
  scale_shape_manual(values = c(1, 19)) +
  labs(x = "Purple Sea Stars per 10m^2",
       y = "Orange Sea Stars per 10m^2") +
  scale_color_brewer(palette = "Paired") +
  theme_bw() 


# Equal_axis_virdis.png - visualizes the difference between the population size ranges
# plots data combined by year
ggplot(all_sites, aes(Purple_per_10m, Orange_per_10m, color = Site, shape = Period)) +
  geom_point() +
  scale_shape_manual(values = c(1, 19)) +
  labs(x = "Purple Sea Stars per 10m^2",
       y = "Orange Sea Stars per 10m^2") +
  scale_color_viridis_d(option = "D") + # C, D, F, and G are all nice. D is best for point
  scale_y_continuous(limit = c(0,4)) +
  scale_x_continuous(limit = c(0,4)) +
  theme_light()

# plots all observations
ggplot(all_species, aes(Purple_per_10m, Orange_per_10m, color = Site, shape = Period)) +
  geom_point() +
  scale_shape_manual(values = c(1, 19)) +
  labs(x = "Purple Sea Stars per 10m^2",
       y = "Orange Sea Stars per 10m^2") +
  scale_color_viridis_d(option = "D") + # C, D, F, and G are all nice. D is best for point
  scale_y_continuous(limit = c(0,4)) +
  scale_x_continuous(limit = c(0,4)) +
  theme_light()

# uses yearly total count instead of density
ggplot(all_sites, aes(Purple_Count, Orange_Count, color = Site, shape = Period)) +
  geom_point() +
  scale_shape_manual(values = c(1, 19)) +
  labs(x = "Total Purple Sea Stars",
       y = "Total Orange Sea Stars") +
  scale_color_viridis_d(option = "D") + # C, D, F, and G are all nice. D is best for point
  theme_linedraw()

# relative population scatter plot
ggplot(all_sites, aes(Purple_relative_population, Orange_relative_population, color = Site, shape = Period)) +
  geom_point() +
  scale_shape_manual(values = c(1, 19)) +
  labs(x = "Purple Sea Star Relative Population",
       y = "Orange Sea Stars Relative Population") +
  scale_color_viridis_d(option = "D") + # C, D, F, and G are all nice. D is best for point
  theme_bw()


# line_virdis uses relative population size
ggplot(all_species, aes(Purple_relative_population, Orange_relative_population, color = Site)) +
  geom_line(aes(linetype = Period, group = interaction(Period, Site)), key_glyph = "timeseries" ) +
  scale_shape_manual(values = c(1, 19)) +
  labs(x = "Purple Sea Stars Relative Population",
       y = "Orange Sea Stars Relative Population") +
  scale_color_viridis_d(option = "B") + 
  theme_light()



# ------- Highlight key points ------
# key points: the last observations before the beginning of the epizootic and the
# first ones after
all_sites$Key_point <- "No"

run_list <- rle(all_sites$Period)
run_lengths <- run_list$lengths
run_positions <- cumsum(run_lengths)
last_idx <- run_positions[run_list$values == "Pre-SSWD"]

all_sites[last_idx, 10] <- "Yes"
all_sites[last_idx +1 , 10] <- "Yes"

# Highlight using color and alpha (opacity)
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


# KeyPoint_Highlighted
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
  guides(labels = label_wrap(18) , alpha = "none", size = "none") +
  theme_bw()


# Larger legend text
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



# ---- smaller dataframe for playing around with color and theme

library(dplyr)
reduced_sites <- bind_rows(read.csv("Cleaned Dataframes/AlmarAvenue.csv"), 
                           read.csv("Cleaned Dataframes/AsilomarStatePark.csv"),
                           read.csv("Cleaned Dataframes/CarmelPoint.csv"),
                           read.csv("Cleaned Dataframes/CoalOilPoint.csv"),
                           read.csv("Cleaned Dataframes/DavenportLanding.csv"))


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





# ------ Density vs mortality plots ----- 
# Plots show no apparent relationship between the two variables

# Morality_plot
ggplot(density_mortality, aes(last_pre_density, Mortality_rate)) +
  geom_point() +
  labs(x = "Last density recorded before SSWD onset (individuals per 10m^2)",
       y = "Ochre Sea Star Mortality Rate (%)") +
  theme_bw()

# Mortality_K_plot
ggplot(density_mortality, aes(k, Mortality_rate)) +
  geom_point() +
  labs(x = "Pre-SSWD Average Population (individuals per 10m^2)",
       y = "Ochre Sea Star Mortality Rate (%)") +
  theme_bw()



# ------- Put data table into long format ---------
library(tidyverse)

long_format <- pivot_longer(all_species, c(ends_with("Abundance"), ends_with("Count")),
                            names_to = "Phenotype_Species", values_to = "Count_Abundance")

# Streamline the data table so it's just the columns I need
by_species <- long_format[,c(1,2,8,9)]
# Hard to plot purple and prey species (observations are 0-25) on the same axis
no_purple <- by_species[c(by_species$Phenotype_Species != "Purple_Count"),]

# All_species_boxplot
ggplot(no_purple, aes(x = Site, y = Count_Abundance, color = Phenotype_Species)) +
  geom_boxplot(aes(group = interaction(Phenotype_Species, Site))) +
  scale_color_discrete(name = "Species", labels = c("Acorn Barnacle Abundance",
                                                    "Mussel Abundance", "Orange Ochre Sea Star Count")) +
  scale_x_discrete(labels = label_wrap(11)) +
  scale_y_continuous(limit = c(0,30)) +
  labs(y ="Count (total individuals)/Abundance (squares present per 1/4m plot)") +
  theme_classic() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.01, 0.99),
        legend.justification = c(0, 1))


# -------- Sea star time series -------- 

# Trial with only one site to work everything out before I introduce the faceting
almar_long <- by_species[by_species$Site == "Almar Avenue" & 
                           (by_species$Phenotype_Species == "Purple_Count" 
                            | by_species$Phenotype_Species == "Orange_Count"), ]

ggplot(almar_long, aes(x = Year, y = Count_Abundance, group = Phenotype_Species, 
                       color = Phenotype_Species)) +
  geom_line() +
  labs(y = "Total Count", color = "Phenotype") +
  scale_color_manual(values = c("orange", "purple"), labels = c("Orange", "Purple")) +
  scale_x_continuous(breaks = seq(2008, 2021, 2)) +
  theme_classic() + 
  theme(legend.position = "inside",
        legend.position.inside = c(0.99, 0.99),
        legend.justification = c(1, 1))


# First attempt...total count is the measure to use for this, too many sites
ggplot(long_stars, aes(x = Year, y = Count_Abundance, group = Phenotype_Species,
                       color = Phenotype_Species)) +
  geom_line() +
  facet_wrap(vars(Site)) +
  labs(y = "Total Count", color = "Phenotype") +
  scale_color_manual(values = c("orange", "purple"), labels = c("Orange", "Purple")) +
  scale_x_continuous(breaks = seq(2008, 2021, 2)) +
  theme_bw() + 
  theme(legend.position = "inside",
        legend.position.inside = c(0.99, 0.99),
        legend.justification = c(1, 1))


# Compare density over time across sites
long_density_stars <- pivot_longer(all_species, ends_with("10m"),
                                   names_to = "Phenotype", values_to = "Density")
# Only the sites with observations spanning 8+ years
site_idx <- long_density_stars$Site != "Carmel Point" & 
  long_density_stars$Site != "Coal Oil Point" & 
  long_density_stars$Site != "Wilder Ranch"
restricted_long_density_stars <- long_density_stars[site_idx,]

# Density_facet
ggplot(restricted_long_density_stars, aes(x = Year, y = Density, group = Phenotype, 
                                          color = Phenotype)) +
  geom_line() +
  facet_wrap(vars(Site)) +
  labs(y = "Density (individuals per 10m^2", color = "Phenotype") +
  scale_color_manual(values = c("orange", "purple"), labels = c("Orange", "Purple")) +
  scale_x_continuous(limits = c(2008, 2023)) +
  theme_bw() + 
  theme(legend.position = "inside",
        legend.position.inside = c(0.99, 0.27),
        legend.justification = c(1, 1))


# Compare relative population size over time across sites
long_relative_stars <- pivot_longer(all_species, ends_with("population"),
                                    names_to = "Phenotype", values_to = "Relative_population")
# Only the sites with observations spanning 8+ years
site_idx <- long_relative_stars$Site != "Carmel Point" & 
  long_density_stars$Site != "Coal Oil Point" & 
  long_density_stars$Site != "Wilder Ranch"
restricted_long_relative_stars <- long_relative_stars[site_idx,]

# Relative_population_facet - vertical dashed line Shows the onset of the SSWD epizootic
ggplot(restricted_long_relative_stars, aes(x = Year, y = Relative_population, group = Phenotype, 
                                           color = Phenotype)) +
  geom_line() +
  facet_wrap(vars(Site)) +
  labs(y = "Density (individuals per 10m^2", color = "Phenotype") +
  scale_color_manual(values = c("orange", "purple"), labels = c("Orange", "Purple")) +
  scale_x_continuous(limits = c(2008, 2023)) +
  geom_vline(xintercept = 2013, linetype = 2) + # 
  theme_bw() + 
  theme(legend.position = "inside",
        legend.position.inside = c(0.99, 0.27),
        legend.justification = c(1, 1))















