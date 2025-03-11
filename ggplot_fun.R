library(ggplot2)
all_sites <- read.csv("AllSites.csv")

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

ggplot(reduced_sites, aes(Purple_per_10m, Orange_per_10m, color = Site, shape = Period)) +
  geom_point() +
  scale_shape_manual(values = c(1, 19)) +
  labs(x = "Purple Sea Stars per 10m^2",
       y = "Orange Sea Stars per 10m^2") +
  scale_color_brewer(palette = "Paired") + # Color-blind friendly. Kinda ugly though.
  theme_bw()

ggplot(reduced_sites, aes(Purple_per_10m, Orange_per_10m, color = Site, shape = Period)) +
  geom_point() +
  scale_shape_manual(values = c(1, 19)) +
  labs(x = "Purple Sea Stars per 10m^2",
       y = "Orange Sea Stars per 10m^2") +
  scale_color_hue(h = c(264, 340), l = 75, direction = 1) + 
  theme_bw()

ggplot(reduced_sites, aes(Purple_per_10m, Orange_per_10m, color = Site, shape = Period, linetype = Site)) +
  geom_line() +
  scale_shape_manual(values = c(1, 19)) +
  labs(x = "Purple Sea Stars per 10m^2",
       y = "Orange Sea Stars per 10m^2") +
  theme_bw()

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
  theme_bw()
