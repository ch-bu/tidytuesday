library(tidyverse)
library(broom)

wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")


model <- lm(points ~ log2(price), data = wine_ratings[1:100, ]) 


model %>%
  augment(data = wine_ratings[1:100, ]) -> d
  ggplot(d) + 
  geom_point(aes(x = .fitted, y = points))

  
wine_ratings %>%
  mutate(
    variety = fct_lump(variety, 10) 
  ) %>%
  drop_na(variety) %>%
  group_by(variety) %>%
  summarise(
    mean_rating = mean(points)
  ) %>%
  arrange(mean_rating)
  
