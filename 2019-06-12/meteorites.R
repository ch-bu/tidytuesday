library(tidyverse)
library(lubridate)
library(hrbrthemes)
library(ggthemes)
library(cowplot)

meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv") %>%
  mutate(
    log_mass = log(mass)
  )

mass_limit <- 10000000

small_impacts <- meteorites %>%
  filter(mass < mass_limit)

huge_impacts <- meteorites %>%
  filter(mass >= mass_limit)

world <- ggplot() + 
  borders("world", colour = "#5b616b", fill = "#5b616b") +
  theme_map() +
  coord_map(projection = "mollweide", orientation = c(90, 0, 0)) +
  geom_point(data = small_impacts, color = "#f9e0de",
             aes(x = long, y = lat, size = mass), alpha = .1) +
  annotate("text", x = -123 - 50, y = 45.4 - 30, color = "#f1f1f1", hjust = 0,
           fontface = "italic",
           label = "In 1902 the meteorite\nWillamette crashed in the US.\nIt was 7.8 square meters long\nand weight 15.5 tons") +
  geom_segment(aes(x = -123, y = 45.4, 
                   xend = -123 - 30, yend = 45.4 - 15), color = "#f1f1f1") +
  annotate("text", x = 17.9 + 20, y = -19.6 - 30, color = "#f1f1f1", hjust = 0,
           fontface = "italic",
           label = "Huba is the biggest meteorite ever\nfound on earth. It weighs 60 tons\nand has landed around\n80,000 years ago") +
  geom_segment(aes(x = 17.9, y = -19.6, 
                   xend = 17.9 + 30, yend = -19.6 - 15), color = "#f1f1f1") +
  geom_point(data = huge_impacts, color = "#dd361c",
             aes(x = long, y = lat, size = mass), alpha = .6) +
  theme(
    plot.title = element_text(color = "#f1f1f1", hjust = 0.5,
                              margin = margin(b = 15),
                              size = 30,
                              face = "bold",
                              family = "Titillium Web"),
    plot.subtitle = element_text(color = "#aeb0b5", hjust = 0.5,
                              margin = margin(b = 35),
                              size = 20,
                              family = "Titillium Web"),
    plot.background  = element_rect(fill  = "#323a45", color = NA),
    panel.background = element_rect(fill  = "#323a45", 
                                    color = "#323a45")
  ) +
  guides(
    size = "none",
    color = "none"
  ) +
  labs(
    title = "Meteorites falling on earth",
    subtitle = "Red dots indicate meteorite impacts with a mass\nbigger than 11 tons"
  )

ggdraw(world) +
  theme(
    plot.background = element_rect(fill = "#323a45"),
    panel.background = element_rect(fill = "#323a45", color = "#323a45"),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  ) 


