library(tidyverse)
library(hrbrthemes)
library(countrycode)
library(gghighlight)
library(cowplot)
# library(extrafont)

theme_set(theme_modern_rc(axis_title_size = 13))

# 
# font_import()
# loadfonts()

student_ratio <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")

world <- map_data("world") %>%
  filter(region != "Antarctica") 


country_data <- student_ratio %>%
  filter(indicator == "Tertiary Education") %>%
  group_by(year, country) %>%
  summarise(
    student_ratio = mean(student_ratio, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  drop_na(student_ratio) %>%
  rename(region = country) %>%
  left_join(world, by = "region")


p <- ggplot() + 
  geom_map(data = world, map = world,
           aes(long, lat, group = group, map_id = region),
           fill = "#282828", color = "#282828") +
  geom_map(data = country_data, map = world,
           aes(fill = student_ratio, map_id = region),
           color = "#282828", size = 0.15, alpha = .8) +
  scale_fill_gradient(low = "#55bee9", high = "#f1b545") +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(x = "", y = "") +
  guides(
    fill = guide_legend(title = "# of students\nper teacher")
  ) +
  coord_map("gilbert", xlim = c(-180, 180)) +
  labs(
    title = '"Oh dear, some teachers have it tough"',
    subtitle = "How the student-to-teacher ratio varies across the globe"
  ) +
  theme(
    text = element_text(family = "Libre Baskerville"),
    plot.title = element_text(color = "#ffffff",
                              margin = margin(t = 30, b = 10),
                              size = 20),
    plot.subtitle = element_text(color = "#ababab",
                              margin = margin(b = 20),
                              size = 15,
                              hjust = 0.7),
    plot.background  = element_rect(fill  = "#323232"),
    panel.background = element_rect(fill  = "#323232", 
                                    color = "#323232"),
    legend.position = "right",
    legend.title = element_text(color = "#6d6d6d",
                                size = 10),
    legend.text = element_text(color = "#6d6d6d",
                               size = 10)
  )


ggdraw(p) +
  theme(
    plot.background = element_rect(fill = "#323232")
  )
