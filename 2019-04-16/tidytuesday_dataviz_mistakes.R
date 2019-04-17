library(tidyverse)
library(ggwaffle)
library(hrbrthemes)
library(cowplot)

theme_set(theme_modern_rc(axis_title_size = 13))

women_research <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/women_research.csv")



make_tibble <- function(number) {
  x <- data_frame(x = seq(1, 10) %>% rep(10),
         y = seq(1, 10) %>% rep(10) %>% sort,
         sex = c(rep("women", number), 
                 rep("men", 100 - number)))
}

# Prepare data
women_research_cleaned <- women_research %>%
  nest(-country, -field) %>% 
  mutate(
    amount = data %>% map_dbl(~ .$percent_women * 100 %>% floor) %>% as.integer
  ) %>%
  select(-data) %>%
  mutate(
    data_frame = amount %>% map(~ make_tibble(.))
  ) %>%
  unnest() %>%
  mutate(amount = amount %>% as.factor %>% fct_inorder) 


plot <- women_research_cleaned %>%
  mutate(
    field = case_when(
      field == "Computer science, maths" ~ "Computer sc.",
      field == "Engineering" ~ "Engineering",
      field == "Health sciences" ~ "Health sc.",
      field == "Physical sciences" ~ "Physical sc.",
      field == "Women inventores" ~ "Women invent."
    )
  ) %>%
  ggplot(aes(x, y, fill = sex)) + 
    geom_tile(color = "#1e1e1e") +
    coord_fixed() +
    facet_grid(field ~ country) +
  labs(
    title = "Still a man's world",
    subtitle = "Women among researchers with papers published 2011-15, & of total in 5 different disciplines",
    x = "",
    y = "",
    fill = "Sex",
    caption = "Sources: 'Gender in the Global Research Landscape' by Elsevier; The Economist"
  ) +
  theme(
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 18, margin = margin(b = 35)),
    axis.text = element_text(color = "#1e1e1e"),
    strip.text = element_text(color = "#ffffff", size = 10),
    panel.grid.major = element_line(color = "#1e1e1e"),
    panel.grid.minor = element_line(color = "#1e1e1e"),
    legend.position = "bottom",
    plot.caption = element_text(face = "italic", color = "grey")
  ) +
  scale_fill_manual(values = wes_palette("Darjeeling2"))

ggdraw(plot) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e")
  )
