library(tidyverse)
library(lubridate)

emperors <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv")



ggplot(emperors, aes(x = birth, y = reorder(name, birth))) +
  geom_segment(aes(
    x = birth,
    xend = death,
    y = name,
    yend = name
  ), color = "steelblue", size = 2, lineend = "round") +
  theme_minimal()
