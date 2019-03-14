library(tidyverse)
library(hrbrthemes)
library(viridis)

theme_set(theme_modern_rc(axis_title_size = 13))

board_games <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv")

most_prolific <- board_games %>%
  count(designer, sort = TRUE) %>%
  drop_na() %>%
  slice(2:6)

most_prolific_data <- board_games %>%
  filter(designer %in% most_prolific$designer) %>%
  group_by(designer, year_published) %>%
  summarise(
    mean_rating = mean(average_rating)
  )

most_prolific_data_first_year <- most_prolific_data %>%
  arrange(designer, year_published) %>%
  group_by(designer) %>%
  slice(1:1) %>%
  ungroup()

board_games %>%
  ggplot(aes(x = year_published, y = average_rating)) +
  geom_jitter(alpha = .04, color = "#cccccc") +
  geom_line(aes(x = year_published, y = mean_rating,
                color = designer), data = most_prolific_data,
            size = 1.4,
            alpha = .8,
             show.legend = FALSE) +
  geom_label(aes(x = year_published, y = mean_rating + 0.3, 
                 fill = designer, 
                label = designer), data = most_prolific_data_first_year,
            show.legend = FALSE,
            color = 'white',
            label.padding = unit(0.45, "lines"),
            label.size = 0,
            alpha = .2,
            fontface = "bold") +
  scale_color_viridis(discrete = TRUE, option = "magma", begin = 0.4) +
  scale_fill_viridis(discrete = TRUE, option = "magma", begin = 0.4) +
  labs(
    x = "Year",
    y = "Average rating",
    title = "Average rating of the\nmost prolific game board game designers",
    caption = "graphic: Christian Burkhart",
    subtitle = "These 5 people have played a major role in the board game industry\nover the past 50 years. The grey points represent the average rating\nof all board games. The color lines the average rating\nof each designer per year."
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "#565656"),
    axis.title.y = element_text(margin = margin(r = 20)),
    axis.title = element_text(colour = "#565656"),
    plot.caption = element_text(vjust = 1, color = "#565656",
                                face = "italic"),
    plot.subtitle = element_text(margin = margin(b = 20))
  )

