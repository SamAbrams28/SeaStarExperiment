library(ggplot2)
two_sites <- read.csv("two_sites.csv")

ggplot(two_sites, aes(Purple, Orange, color = Site, shape = SSWD)) +
  geom_point()
