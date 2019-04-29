library(tidyverse)
library(hrbrthemes)
library(lubridate)

theme_set(theme_ipsum_rc())

tidy_anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")

# Gantt chart of most 20 most famous anime

anime_data <- tidy_anime %>%
  nest(-name) %>%
  mutate(
    score = data %>% map_dbl(~ mean(.$score, na.rm = TRUE))
  ) %>%
  arrange(desc(score)) %>%
  slice(1:20) %>%
  unnest() %>%
  drop_na(start_date, end_date) %>%
  group_by(name) %>%
  slice(1:1) %>%
  ungroup() %>%
  select(name, score, start_date, end_date)

anime_tidied <- tidy_anime %>%
  mutate(
    year = start_date %>% year
  ) 

anime_tidied %>%
  ggplot(aes(x = year, y = score, group = year)) +
  geom_jitter(color = "#ececec") +
  geom_boxplot(outlier.shape = NA, outlier.size = 0, coef = 0,
               alpha = .9, fill = "#F5E1DA") +
  labs(
    title = "Anime Movies are getting better",
    subtitle = "Since the 1960s, Anime Movies have continuously improved in terms of user ratings"
  ) +
  theme(
    text = element_text(family = "FreeSans"),
    plot.title = element_text(family = "FreeSans"),
    plot.subtitle = element_text(family = "FreeSans"),
    axis.title.y = element_text(margin = margin(r = 10), 
                                family = "FreeSans"),
    axis.title.x = element_text(family = "FreeSans")
  )
