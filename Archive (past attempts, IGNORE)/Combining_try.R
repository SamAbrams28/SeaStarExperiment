setwd("/Users/samanthaabrams/Documents/MarinCS\ 100B/SeaStarExperiment/Cleaned\ Dataframes")
almar <- read.csv("AlmarAvenue.csv")
asilomar <- read.csv("AsilomarStatePark.csv")
carmel <- read.csv("CarmelPoint.csv")

manual_first_three <- data.frame(Site = c(almar[,1], asilomar[,1], carmel[,1]),
                          Purple.count = c(almar[,2], asilomar[,2], carmel[,2]),
                          Purple.per.10.m.2 = c(almar[,3], asilomar[,3], carmel[,3]),
                          Orange.count = c(almar[,4], asilomar[,4], carmel[,4]),
                          Orange.per.10.m.2 = c(almar[,5], asilomar[,5], carmel[,5]),
                          Period = c(almar[,6], asilomar[,6], carmel[,6])
)

library(ggplot2)
ggplot(first_three, aes(Purple.count, Orange.count, color = Site, shape = Period)) +
  geom_point() +
  scale_shape_manual(values = c(1, 19))


# Determine how many total sampling events there are in the whole data set
length_first_three <- length(almar[,1]) + length(asilomar[,1]) + length(carmel[,1])

# Giant data frame where everything will go
first_three <- data.frame(Site = rep("x", length_first_three),
                          Purple.count = rep(0, length_first_three),
                          Purple.per.10.m.2 = rep(0, length_first_three),
                          Orange.count = rep(0, length_first_three),
                          Orange.per.10.m.2 = rep(0, length_first_three),
                          Period = rep("x", length_first_three))

# Loop to fill in all the columns
for (i in 1:6) {
  first_three[,i] = c(almar[,i], asilomar[,i], carmel[,i])
}
