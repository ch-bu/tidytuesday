library(tidyverse)
library(hrbrthemes)
library(emojifont)
library(stringr)

bird_counts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-18/bird_counts.csv")

total_count_per_year <- bird_counts %>%
  mutate(
    year = year - year %% 5
  ) %>%
  group_by(year) %>%
  summarise(
    total_count = sum(how_many_counted_by_hour)
  )

european_starling <- bird_counts %>%
  mutate(
    year = year - year %% 5
  ) %>%
  filter(species %in% c("European Starling")) %>%
  group_by(year) %>%
  summarise(
    counted = sum(how_many_counted_by_hour)
  ) %>%
  left_join(total_count_per_year, by = "year") %>%
  mutate(
    label = fontawesome('fa-twitter')
  ) %>%
  drop_na()

text_color = "#939599"

european_starling %>%
  ggplot(aes(x = year, y = total_count)) +
  geom_segment(
    aes(x = year, xend = year, y = counted, yend = total_count), colour = "#52565c"
  ) +
  geom_text(aes(label = label), family='fontawesome-webfont', size = 10,
            color = "#fcf594") +
  geom_text(aes(x = year, y = total_count, label = round(total_count, 0)), 
            family='Open Sans', size = 5, nudge_x = 0, 
            nudge_y = 220, color = text_color) +
  geom_text(data = european_starling, aes(x = year, y = counted, label = label), 
            family='fontawesome-webfont', color = "#ff7a8a", size = 10) +
  geom_text(data = european_starling, aes(x = year, y = counted, label = round(counted, 0)), 
            family='Open Sans', size = 5, nudge_x = 0, 
            nudge_y = -220, color = text_color) +
  annotate("text", x = 1967, y = 2774,
           hjust = 0,
           fontface = "italic", label = "Yellow birds indicate the total\nnumber of birds counted per\nhour in a given that year",
           color = "#fcf594") +
  annotate("text", x = 1980, y = 200,
           hjust = 0,
           fontface = "italic", label = "Red birds indicate the total number of\nEuropean Starlings counted per\nhour in a given that year",
           color = "#ff7a8a") +
  labs(
    title = str_to_title("The European Starling - the celebrity among the birds"),
    subtitle = "The European Starling is one of the most common birds. In 2004 there were about\n310 million individuals occupying an area of 8,870,000 square kilometer. That is\nabout the largest size of the Roman Empire.",
    x = "",
    y = "Average total count per hour"
  ) +
  scale_x_continuous(breaks = seq(from = 1950, to = 2015, by = 5 )) +
  theme_modern_rc() +
  theme(
    plot.background = element_rect(fill = "#282c34"),
    panel.background = element_rect(fill = "#282c34", color = "#282c34"),
    plot.title = element_text(margin = margin(b = 10), 
                              color = "#ffffff",
                              size = 23,
                              family = "Open Sans"),
    plot.subtitle = element_text(margin = margin(b = 65), 
                                 color = "#bebfc2",
                                 size = 18,
                                 family = "Open Sans"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "#686b70", margin = margin(t = 20)),
    axis.ticks.x = element_line(color = "#3d4148")
  )

         
  
  
  
  
  
  
  
  
  













