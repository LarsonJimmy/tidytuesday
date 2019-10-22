# Module 8 advanced data viz
# created: 10/22/19
# author: Jimmy Larson
#

library(tidyverse)
library(RColorBrewer)

# read in the data
wine <- read_csv("data/2019/2019-05-28/winemag-data-130k-v2.csv")
# wine rating by US state ----
wine %>%
  filter(country == "US") %>%
  group_by(province) %>%
  summarise(
    count = n(),
    mean_prov = mean(points),
    std = sd(points)) %>%
  filter(count > 100) %>%
  mutate(coast = c("West", "West", "East", "East", "West", "East", "West")) %>%
ggplot(., aes(province, mean_prov, fill = coast))+
  geom_bar(stat = "identity", color = "black")+
  scale_fill_brewer(palette = "Set2")+
  geom_errorbar(aes(ymin = mean_prov - std, ymax = mean_prov + std),
                position = "dodge",
                width = 0.2)+
  labs(
    x = "State",
    y = "Average Rating",
    fill = "Coast",
    title = "Average Wine Rating by U.S. State",
    subtitle = "From states with more than 100 wines rated" 
  ) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = -90, hjust = 0,vjust = 1))
## save the plot
ggsave("wine_rating_by_state_tidy_tuesday_jimmy_larson.pdf")

# Cost and rating of three highest rated states ----
wine %>%
  filter(province == c("California", "Oregon", "Washington")) %>%
  ggplot(., aes(x = points, y= price))+
  geom_point(alpha=1/10)+
  facet_wrap(~province)+
  labs(
    x = "Rating",
    y = "Price ($)",
    title = "Price vs. Rating per Bottle",
    subtitle = "From the three highest rated states"
  )+
  theme_bw()
## save the plot
ggsave("wine_cost_rating_tidy_tuesday_jimmy_larson.png")

# ugly plot ----
wine %>%
  filter(province == "California",
         price > 100) %>%
  ggplot(., aes(points, price))+
  geom_point(aes(colour = taster_twitter_handle))+
  scale_color_brewer(palette = "Blues")+
  theme(panel.background = element_rect(fill = "pink", color = "purple", size = 2),
        plot.background = element_rect(fill = "pink"),
        legend.position = c(.5, .6))
##save the plot
ggsave("ugly_plot_wine_tidy_tuesday_jimmy_larson.pdf")
  
