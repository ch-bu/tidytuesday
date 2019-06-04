library(tidyverse)
library(broom)
library(hrbrthemes)

theme_set(theme_modern_rc(axis_title_size = 13))

ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")

ramen_ratings_processed <- ramen_ratings %>%
  mutate(
    style = fct_lump(style, 4),
    country = fct_lump(country, 12),
    brand = fct_lump(brand, 20)
  ) %>%
  drop_na(style)

model_terms <- lm(stars ~ brand + country + style, ramen_ratings_processed) %>%
  tidy(conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  arrange(desc(estimate)) %>%
  extract(term, c("category", "term"), "^([a-z]+)([A-Z].*)") 

grey_color <- "#e7e7e7"
soft_grey <- "#7e7e7e"
supersoft_grey <- "#dbdbdb"
text_color <- "#22222b"
green_color <- "#00c18d"

model_terms %>%
  filter(category == "country") %>%
  mutate(term = fct_reorder(term, estimate)) %>%
  rename(country = term) %>%
  ggplot(aes(estimate, country)) +
  geom_point(color = green_color, size = 4) +
  geom_vline(xintercept = 0, lty = 2, color = soft_grey) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 color = green_color, height = .2) +
  annotate("text", color = "#383840", x = -0.45, y = 10.3,
           hjust = 0,
           fontface = "italic",
           label = "The estimates to the right of\nthis value indicate an overall\npositive relationship with\nthe ramen rating.\nValues on the left indicate\nnegative relationship.") +
  geom_curve(aes(x = -0.02, y = 10.3, xend = -0.1, yend = 10.1),
             curvature = -0.2, color = soft_grey, size = 0.05) +
  labs(
    x = "Regression estimates",
    y = "Country of origin",
    title = "How does the country of origin\naffects the rating of ramens?",
    subtitle = "Regression coefficients predicting ramen\nratings by country of origin"
  ) +
  scale_color_continuous() +
  theme(
    plot.title = element_text(margin = margin(b = 10), 
                              color = text_color,
                              size = 22,
                              family = "Open Sans"),
    plot.subtitle = element_text(margin = margin(b = 45), 
                                 color = text_color,
                                 size = 17,
                                 family = "Open Sans"),
    axis.title.x = element_text(margin = margin(t = 15),
                                color = soft_grey),
    axis.text.x    = element_text(color = text_color,
                                  margin = margin(t = 15)),
    axis.text.y    = element_text(color = text_color),
    axis.title.y = element_text(margin = margin(r = 15),
                                color = soft_grey),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.major.y = element_line(color = supersoft_grey),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = grey_color),
    plot.margin = unit(c(1, 2, 1, 1), "cm")
  )



