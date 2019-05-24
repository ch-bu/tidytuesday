library(tidyverse)
library(janitor)
library(ggrepel)


coast_vs_waste <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/coastal-population-vs-mismanaged-plastic.csv")
mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")
waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")

waste_data <- waste_vs_gdp %>%
  clean_names() %>%
  setNames(c("entity", "code", "year", "waste",
             "gdp", "population")) %>%
  drop_na(waste, gdp)

mismanaged_data <- mismanaged_vs_gdp %>%
  clean_names() %>%
  setNames(c("entity", "code", "year", "mismanaged_plastic",
             "gdp", "population")) %>%
  drop_na(mismanaged_plastic, gdp)

data <- waste_data %>%
  full_join(mismanaged_data) %>%
  mutate(
    waste_ratio = mismanaged_plastic / waste
  ) 

good_guys_data <- tibble(x = seq(0, 0.75, by = .001),
                          y = x * .4)
medium_guys_data <- tibble(x = seq(0, 0.75, by = .001),
                           ymin = x * .4,
                           ymax = x * .7)
bad_guys_data <- tibble(x = seq(0, 0.75, by = .001),
                           ymin = x * .7,
                           ymax = x * 1)

data_bad_countries <- data %>%
  filter(waste_ratio > .7, waste > 0.2)

data_good_guys <- data %>% 
  filter(waste_ratio < .4, waste > 0.5, waste < 0.8)

ggplot(data, aes(x = waste, y = mismanaged_plastic)) + 
  # geom_abline(slope = 1, color = "#e5e5e5") +
  # geom_abline(slope = .7, color = "#E5E5E5") +
  # geom_abline(slope = .4, color = "#E5E5E5") +
  geom_ribbon(mapping = aes(x = x, ymax = y, ymin = 0), fill = "#fceaea",
              data = good_guys_data, inherit.aes = FALSE,
              alpha = .6) +
  geom_ribbon(mapping = aes(x = x, ymax = ymax, ymin = ymin), fill = "#fad5d5",
              data = medium_guys_data, inherit.aes = FALSE,
              alpha = .6) +
  geom_ribbon(mapping = aes(x = x, ymax = ymax, ymin = ymin), fill = "#e72d2d",
              data = bad_guys_data, inherit.aes = FALSE,
              alpha = .7) +
  geom_point(aes(size = gdp), show.legend = FALSE, alpha = .8, color = "#170404") +
  annotate("text", family = "Open Sans", color = "#170404",
           x = 0.41, y = 0.39,  hjust = 0, fontface = 2, size = 7,
           label = "70% to 100%") +
  annotate("text", family = "Open Sans",
           x = 0.53, y = 0.29,  hjust = 0, fontface = 2, size = 7,
           label = "40% to 70%") +
  annotate("text", family = "Open Sans",
           x = 0.60, y = 0.13,  hjust = 0, fontface = 2, size = 7,
           label = "0% to 40%") +
  geom_text_repel(family = "Open Sans", aes(label = entity), 
                  fontface = "italic",
                  data = data_bad_countries, color = "#170404") +
  geom_text_repel(family = "Open Sans", aes(label = entity), 
                  fontface = "italic",
                  data = data_good_guys, color = "#170404") +
  coord_cartesian(ylim = c(0, 0.5), xlim = c(0, 0.75)) +
  scale_x_continuous(labels = function(x) paste(x, "kg")) +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    plot.margin = unit(c(1,1,1,1), "cm"),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.x = element_text(size = 12),
    axis.ticks.x = element_blank(),
    axis.title.x = element_text(margin = margin(t = 15, b = 15),
                                size = 12, family = "Open Sans"),
    plot.title = element_text(family = "Open Sans", face = "bold",
                              hjust = 0, margin = margin(t = 20, b = 10), size = 30),
    plot.subtitle = element_text(family = "Open Sans", color = "#515151",
                                 hjust = 0, margin = margin(b = -140), size = 23)
  ) +
  labs(
    x = "Amout of plastic waste per capita in kg/day",
    title = "Who doesn't care\nabout plastic waste?",
    subtitle = "Percentage of plastic waste\nthat is not properly disposed of,\nper country"
  )







