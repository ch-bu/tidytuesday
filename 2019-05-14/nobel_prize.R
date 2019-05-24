library(tidyverse)
library(hrbrthemes)
library(lubridate)
library(gghighlight)

theme_set(theme_ipsum_rc())

nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")
nobel_winner_all_pubs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winner_all_pubs.csv")


# Wie lange braucht es für die Gewinner, einen Nobelpreis zu bekommen?

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
  geom_point(color = "#cccccc") +
  geom_smooth(aes(group = category), color = "#cccccc", se = FALSE) +
  geom_point(data = peace_prices, 
             aes(x = prize_year, y = age_won), color = "blue") +
  geom_smooth(data = peace_prices, 
              aes(x = prize_year, y = age_won), color = "blue", se = FALSE) +
  labs(
    x = "Prize year",
    y = "Age won"
  ) 








