# An analysis of the contents in works painted by Bob Ross
# Author: Jimmy Larson
# Created: 9.15.19
# Last edited: 9.15.19

# load packages ----
library(tidyverse)
library(ggthemes)
library(reshape)

# read in data ----
ross <- read_csv("data/2019/2019-08-06/bob-ross.csv")

# filter data by water type ----
lake <- ross[!(ross$LAKE == '0'),]

ocean <- ross[!(ross$OCEAN == '0'),]

river <- ross[!(ross$RIVER == '0'),]

waterfall <- ross[!(ross$WATERFALL == '0'),]

# adding label by water type ----
lake <- lake %>% mutate(type = "lake")

ocean <- ocean %>% mutate(type = "ocean")

river <- river %>% mutate(type = "river")

waterfall <- waterfall %>% mutate(type = "waterfall")

# finding total and % plant type in each data set ----
lake_sums <- as.data.frame(colSums(lake [,c("CACTUS", "CONIFER", "DECIDUOUS", "BUSHES", "FLOWERS", "GRASS", "PALM_TREES")]),col.names =c("sums"))
names(lake_sums)[1] <- "sums"
lake_sums$plant <- rownames(lake_sums)
lake_sums <- lake_sums %>% mutate(total = 143)
lake_sums$per_plant <- (lake_sums$sums / lake_sums$total) * 100
lake_sums <- lake_sums %>% mutate(type = "lake")

ocean_sums <- as.data.frame(colSums(ocean [,c("CACTUS", "CONIFER", "DECIDUOUS", "BUSHES", "FLOWERS", "GRASS", "PALM_TREES")]),col.names =c("sums"))
names(ocean_sums)[1] <- "sums"
ocean_sums$plant <- rownames(ocean_sums)
ocean_sums <- ocean_sums %>% mutate(total = 36)
ocean_sums$per_plant <- (ocean_sums$sums / ocean_sums$total) * 100
ocean_sums <- ocean_sums %>% mutate(type = "ocean")

river_sums <- as.data.frame(colSums(river [,c("CACTUS", "CONIFER", "DECIDUOUS", "BUSHES", "FLOWERS", "GRASS", "PALM_TREES")]),col.names =c("sums"))
names(river_sums)[1] <- "sums"
river_sums$plant <- rownames(river_sums)
river_sums <- river_sums %>% mutate(total = 126)
river_sums$per_plant <- (river_sums$sums / river_sums$total) * 100
river_sums <- river_sums %>% mutate(type = "river")

waterfall_sums <- as.data.frame(colSums(waterfall [,c("CACTUS", "CONIFER", "DECIDUOUS", "BUSHES", "FLOWERS", "GRASS", "PALM_TREES")]),col.names =c("sums"))
names(waterfall_sums)[1] <- "sums"
waterfall_sums$plant <- rownames(waterfall_sums)
waterfall_sums <- waterfall_sums %>% mutate(total = 39)
waterfall_sums$per_plant <- (waterfall_sums$sums / waterfall_sums$total) * 100
waterfall_sums <- waterfall_sums %>% mutate(type = "waterfall")

ross_water <- rbind(lake_sums, ocean_sums, river_sums, waterfall_sums)

# plot ----
ggplot(data = ross_water)+
  geom_col(mapping = aes(x = type, y = per_plant, fill = plant), color = "black", position = "dodge")+
  labs(y = "Plant Type in % of Paintings",
       x = "Body of Water",
       title = "Bob Ross- Happy Little Plants by Water Type",
       subtitle = "Percent of Paintings Each Plant Type is in by Body of Water",
       fill = "Plant Type")+
  theme_bw()

# saving plot ----
ggsave("bob_ross_tidy_tues_plot_jimmy_larson.pdf")
