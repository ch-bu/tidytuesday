library(tidyverse)
library(janitor)

bob_ross <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv")


bob_cleaned <- bob_ross %>% 
  janitor::clean_names() %>% 
  separate(episode, into = c("season", "episode"), sep = "E") %>% 
  mutate(season = str_extract(season, "[:digit:]+")) %>% 
  mutate_at(vars(season, episode), as.integer) %>% 
  gather(type, present, -season, -episode, -title)

(count_objects_data <- bob_cleaned %>% 
    filter(present == 1) %>% 
    mutate(
      type = case_when(
        type == "trees" ~ "tree",
        type == "mountains" ~ "mountain",
        TRUE ~ as.character(type)
      )
    ) %>% 
    mutate(
      type = type %>% fct_lump(19)
    ) %>% 
    count(season, type))

bob_color_one <- "#221B15"
bob_color_two <- "#bcbab8"
grid_color <- "#4e4843"

count_objects_data %>% 
  ggplot(aes(x = season, y = n)) + 
  geom_line(color = "#FFEC00", size = 1.1) +
  facet_wrap(~ type, ncol = 5) + 
  labs(
    title = str_to_title("The evolution of bob ross"),
    subtitle = str_to_title("Bob Ross stayed true to himself over the years.\nIn more than 30 series he drew different parts of nature equally often"),
    caption = "source: 538 R package https://github.com/rudeboybert/fivethirtyeight",
    x = "Season",
    y = "Frequency"
  ) +
  theme_minimal() +
  scale_y_continuous(minor_breaks = NULL, 
                     breaks = c(5, 10, 15, 20)) +
  theme(
    plot.background = element_rect(fill = bob_color_one),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.title = element_text(family = "Broadway",
                              size = 28,
                              color = bob_color_two),
    plot.subtitle = element_text(family = "Broadway",
                                 margin = margin(b = 20),
                                 size = 15,
                                 color = bob_color_two),
    strip.text = element_text(family = "Satisfy",
                              size = 13,
                              color = "#e8e8e7"),
    strip.background = element_rect(fill = "#110d0a", color = "#1e1812"),
    panel.background = element_rect(fill = "#38312c", color = "#1e1812"),
    axis.title = element_text(family = "Roboto",
                              color = "#4e4843"),
    plot.caption = element_text(family = "Roboto",
                                size = 8,
                                color = "#4e4843",
                                margin = margin(t = 15)),
    axis.text = element_text(family = "Roboto", color = "#4e4843"),
    axis.title.x = element_text(margin = margin(t = 15), hjust = 1),
    axis.title.y = element_text(margin = margin(r = 15), hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = grid_color),
    panel.grid.minor.y = element_line(color = grid_color)
  )

