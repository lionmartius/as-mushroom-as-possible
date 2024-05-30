# What do we need for successful woodland expansion?
# - As Mushroom as possible
# Ectomycorrhizal grove formation and implications for woodland expansion 
# Intensive sampling - Distances between seedlings and EcM nurse-plants
# Lion R. Martius
# Edinburgh, 25/03/2024

msc <- read.csv(file = 'data/intensive_sampling_distances.csv') # Load data
msc$species <- as.factor(msc$species)
msc$species2 <- paste(msc$genus, msc$species, sep = ' ') # Create species column from genus and species columns 


# Plot distances from nearest Salix spp.
library(ggplot2)
ggplot(data = msc, mapping = aes(x=species2, y = dist_salix, colour = species2))+
  theme_bw()+
  geom_violin(aes(fill = species2), show.legend = F)+
  geom_boxplot(width = 0.1, aes(col = species2), show.legend = F, fatten = 0.4)+
  geom_point(stat = "summary", fun = "median", shape = 23, size = 2.2, 
             aes(fill = species2), show.legend = F) +
  labs(x = NULL) +
  scale_color_manual(values = c('Betula pubescens'= 'slategrey', 
                                'Pinus sylvestris'='darkseagreen4'))+
  scale_fill_manual(values = c('Betula pubescens'= 'grey83', 
                               'Pinus sylvestris'='#9DC183'))+
  ylab(expression(paste('Distance from',italic(' Salix'),' spp. [cm]')))+
  scale_x_discrete(labels = expression(italic("Betula pubescens"), italic("Pinus sylvestris")))
ggsave(filename = 'distances.png',dpi = 300, width = 6, height = 4)


summary(msc$dist_salix[msc$species2 == 'Betula pubescens'], na.rm = T)
summary(msc$dist_salix[msc$species2 == 'Pinus sylvestris'], na.rm = T)
summary(msc$dist_salix, na.rm = T)

total_observations <- length(msc$dist_salix[!is.na(msc$dist_salix)])

# Percentage of observations where dist_salix < 200
percentage_less_than_200 <- sum(msc$dist_salix < 200, na.rm = TRUE) / total_observations * 100

# Percentage of observations where dist_salix < 100
percentage_less_than_100 <- sum(msc$dist_salix < 100, na.rm = TRUE) / total_observations * 100

percentage_less_than_200
percentage_less_than_100
