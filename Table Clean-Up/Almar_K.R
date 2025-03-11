setwd("/Users/samanthaabrams/Documents/MarinCS\ 100B/SeaStarExperiment/Cleaned\ Dataframes/")
almar <- read.csv("AlmarAvenue.csv")

purple_k <- mean(almar[almar$Period == "Pre-SSWD", 2])
almar$Purple_relative_count <- 0
almar[,8] <- round(almar[,2]/purple_k, 2)

orange_k <- mean(almar[almar$Period == "Pre-SSWD", 4])
almar$Orange_relative_count <- 0
almar[,9] <- round(almar[,5]/orange_k, 2)

library(ggplot2)
ggplot(almar, aes(Purple_relative_count, Orange_relative_count, color = Site, shape = Period)) +
  geom_point() +
  scale_shape_manual(values = c(1, 19))
