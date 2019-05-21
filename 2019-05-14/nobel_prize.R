library(tidyverse)
library(hrbrthemes)
library(lubridate)
library(gghighlight)
library(emojifont)
library(extrafont)

theme_set(theme_ipsum_rc())

# font_import()
# loadfonts()

nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")
# nobel_winner_all_pubs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winner_all_pubs.csv")


winners <- nobel_winners %>%
  filter(laureate_type == "Individual") %>%
  select(prize_year, category, full_name, birth_date) %>%
  mutate(
    birth_year = year(birth_date),
    age_won    = prize_year - birth_year
  ) %>%
  select(-birth_date)

peace_prices <- winners %>%
  filter(category == "Peace")


winners %>%
  ggplot(aes(x = prize_year, y = age_won)) +
  geom_point(color = "#ededed") +
  geom_smooth(aes(group = category), color = "#e999d0", 
              se = FALSE, linetype = "dashed") +
  geom_point(data = peace_prices, 
             aes(x = prize_year, y = age_won), color = "#b2dbee") +
  geom_smooth(data = peace_prices, 
              aes(x = prize_year, y = age_won), fill = "#cce7f4", 
              color = "#008AC8", se = TRUE) +
  labs(
    x = "Award year",
    y = "Age won",
    title = "Nobel Peace Price Winners ☮ Get Younger ️",
    subtitle = "Whereas the nobel price winners of all other\ncategories got older since 1900 the peace price\nwinners got younger and younger"
  ) +
  theme(
    plot.title = element_text(family = "Arapey", hjust = 0.5,
                              size = 25),
    plot.subtitle = element_text(family = "Arapey", 
                                 hjust = 0.5, size = 18),
    axis.title.x = element_text(family = "Arapey", hjust = 0.5,
                                size = 15, margin = margin(t = 15)),
    axis.title.y = element_text(family = "Arapey", hjust = 0.5,
                                size = 15, margin = margin(r = 15)),
    axis.text  = element_text(family = "Arapey"),
    axis.text.x  = element_text(size = 15),
    axis.text.y  = element_text(size = 15),
    panel.grid.major = element_line(color = "#eaeaea"),
    panel.grid.minor = element_line(color = "#eaeaea")
  )








