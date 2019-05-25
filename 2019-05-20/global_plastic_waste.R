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
  geom_ribbon(mapping = aes(x = x, ymax = ymax, ymin = ymin), fill = "#f39696",
              data = medium_guys_data, inherit.aes = FALSE,
              alpha = .6) +
  geom_ribbon(mapping = aes(x = x, ymax = ymax, ymin = ymin), fill = "#e72d2d",
              data = bad_guys_data, inherit.aes = FALSE,
              alpha = .7) +
  annotate("segment", x = 0.223, xend = 0.223, y = 0, yend = 0.178, 
           color = "#515151", linetype = 2) +
  annotate("segment", x = 0.223, xend = 0.75, y = 0.178, yend = 0.184, 
           color = "#515151", linetype = 2) +
  geom_point(show.legend = FALSE, alpha = .8, size = 3,
             color = "#969696", pch = 21, fill = "#cacaca") +
  annotate("text", family = "Open Sans", color = "#2e0909",
           x = 0.41, y = 0.39,  hjust = 0, fontface = 2, size = 7,
           label = "70% to 100%") +
  annotate("text", family = "Open Sans",
           x = 0.53, y = 0.30,  hjust = 0, fontface = 2, size = 7,
           label = "40% to 70%") +
  annotate("text", family = "Open Sans",
           x = 0.60, y = 0.15,  hjust = 0, fontface = 2, size = 7,
           label = "0% to 40%") +
  annotate("segment", x = 0.17, xend = 0.21, y = 0.22, yend = 0.19, 
           color = "#515151") +
  annotate("text", family = "Open Sans",
           x = 0.06, y = 0.25,  hjust = 0, size = 3.5,
           label = "For example, a person in Tonga\nproduces 0.22 kg of\nplastic waste per day,\n0.18 kg of which is not\nproperly disposed of") +
  geom_text_repel(family = "Open Sans", aes(label = entity), 
                  fontface = "italic",
                  data = data_bad_countries, color = "#170404") +
  geom_text_repel(family = "Open Sans", aes(label = entity), 
                  fontface = "italic",
                  data = data_good_guys, color = "#170404") +
  coord_cartesian(ylim = c(0, 0.5), xlim = c(0, 0.72)) +
  scale_x_continuous(labels = function(x) paste(x, "kg")) +
  scale_y_continuous(labels = function(x) paste(x, "kg"), position = "right") +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(family = "Open Sans", face = "bold",
                              hjust = 0, margin = margin(t = 20, b = 10), size = 30),
    plot.subtitle = element_text(family = "Open Sans", color = "#515151",
                                 hjust = 0, margin = margin(b = -140), size = 23),
    plot.margin = unit(c(1, 2, 1, 1), "cm"),
    axis.text.y.right = element_text(size = 10, family = "Open Sans", margin = margin(r = 20)),
    axis.text.x = element_text(size = 10, family = "Open Sans"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(margin = margin(t = 15, b = 15),
                                color = "#515151", hjust = 1,
                                size = 10, family = "Open Sans"),
    axis.title.y = element_text(size = 10, family = "Open Sans", 
                                color = "#515151", hjust = 0)

  ) +
  labs(
    x = "Amout of plastic waste per capita in kg/day",
    y = "Not properly disposed plastic waste per capita in kg/day",
    title = "Who doesn't care\nabout plastic waste?",
    subtitle = "Percentage of plastic waste\nthat is not properly disposed of,\nper country"
  )







