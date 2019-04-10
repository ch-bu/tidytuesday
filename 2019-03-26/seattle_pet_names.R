library(tidyverse)
library(hrbrthemes)
library(lubridate)
library(extrafont)
library(fontawesome)

# theme_set(theme_modern_rc(axis_title_size = 13))


pets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-26/seattle_pets.csv") %>%
  mutate(
    issue_date = license_issue_date %>% mdy(.),
    year = year(issue_date),
    month = month(issue_date)
  )


# Did the most popular names change over the years?
pets %>% 
  count(animals_name, year, sort = TRUE) %>%
  drop_na() %>%
  filter(animals_name %in% c("Lucy", "Charlie", "Luna", "Bella")) %>%
  ggplot(aes(x = animals_name, y = year)) + 
  geom_count(aes(size = n)) 


# What where the most popular cat names in 2018?
fav_names_month <- pets %>%
  count(animals_name, month, sort = TRUE) %>%
  drop_na() %>%
  group_by(month) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  mutate(
    sum = sum(n)
  ) %>%
  ungroup() %>%
  mutate(
    proportion = n / sum ,
    angle = proportion * 360
  )


pets %>%
  count(animals_name, sort = TRUE) %>%
  slice(1:10) %>%
  drop_na()


ggplot(fav_names_month, aes(x = month, y = animals_name)) +
  geom_point(color = "white") +
  geom_spoke(aes(angle = angle, radius = .5, color = angle)) +
  scale_color_manual()
  scale_color_viridis()



