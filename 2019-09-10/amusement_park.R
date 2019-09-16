library(tidyverse)
library(lubridate)
library(cowplot)
library(hrbrthemes)

tx_injuries <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/tx_injuries.csv")
safer_parks <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/saferparks.csv")

tx_injuries_cleaned <- tx_injuries %>% 
  mutate(
    date = case_when(
      injury_date %>% str_detect("n/a") ~ NA_character_,
      injury_date %>% str_detect("#") ~ NA_character_,
      injury_date %>% str_detect("/") ~ mdy(injury_date) %>% as.character,
      TRUE ~ as.Date(as.integer(injury_date), origin = "1900-01-01") %>% 
        as.character
    ) %>% ymd,
    year = year(date),
    quarter = quarter(date),
    age = case_when(
      age %>% str_detect("/") ~ NA_character_,
      age == "mid-60s" ~ "65",
      age == "0" ~ NA_character_,
      TRUE ~ age
    ) %>% as.integer,
    body_part = body_part %>% fct_lump(10) %>% fct_infreq,
    name_of_operation = str_trunc(name_of_operation, 29)
  )

# # Welche Körperteile sind die schlimmsten? 
# tx_injuries_cleaned %>% 
#   mutate(
#     body_part = body_part %>% fct_lump(15) %>% fct_infreq
#   ) %>% 
#   ggplot(aes(x = body_part)) +
#   geom_bar()
# 
# # In welchem Alter gibt es die Verletzungen?
# tx_injuries_cleaned %>% 
#   mutate(
#     body_part = body_part %>% fct_lump(8) %>% fct_infreq
#   ) %>% 
#   drop_na(age, body_part) %>% 
#   filter(age != 0) %>% 
#   count(age, body_part, sort = T) %>% 
#   ggplot(aes(age, n, group = 1)) +
#   geom_line() +
#   facet_wrap(~ body_part)
#   
# # Wann haben die Menschen Verletzungen?
# tx_injuries_cleaned %>% 
#   count(year, quarter) %>% 
#   ggplot(aes(quarter, n)) +
#   geom_col() +
#   facet_wrap(~ year)
# 
# # Was sind die schlimmsten Attraktionen?
# tx_injuries_cleaned %>% 
#   count(name_of_operation, sort = TRUE) %>%
#   head(20) %>% 
#   ggplot(aes(reorder(name_of_operation, n), n)) +
#   geom_col() +
#   coord_flip() 
# 
# 
# 
# nested <- tx_injuries_cleaned %>% 
#   nest(-name_of_operation) %>% 
#   mutate(
#     mean_age = data %>% map_dbl(~ mean(.$age, na.rm = TRUE)),
#     injuries = data %>% map_dbl(~ nrow(.))
#   ) %>% 
#   arrange(desc(injuries))


worst_parks <- tx_injuries_cleaned %>% 
  count(name_of_operation, sort = TRUE) %>% 
  slice(1:15) %>% 
  {.$name_of_operation}
  
  
grouped <- tx_injuries_cleaned %>% 
  select(name_of_operation, body_part) %>% 
  count(name_of_operation, body_part) %>% 
  mutate(
    name_of_operation = fct_infreq(name_of_operation) %>% 
      fct_rev
  ) %>% 
  drop_na(body_part)
  # mutate(name_of_operation = name_of_operation %>% as.character) %>% 
  # mutate(name_of_operation = str_trunc(name_of_operation, 20)) 

heatmap <- grouped %>% 
  filter(name_of_operation %in% worst_parks) %>% 
  ggplot(aes(x = body_part, y = name_of_operation)) +
  geom_tile(aes(fill = n), color = "#2b2b2b") +
  geom_text(aes(label = n), color = "#22292F") +
  # coord_flip() +
  theme_minimal() +
  theme_ft_rc() +
  scale_fill_gradient(low = "#fee8c8", high = "#e34a33") +
  labs(y = "", x = "Body Parts") +
  scale_x_discrete(position = "top") +
  guides(fill = NULL) +
  theme(
    plot.margin = unit(c(1, 0, 0, 1), "cm"),
    axis.title.x = element_text(color = "#8795A1", size = 15,
                                # margin = margin(l = 30, b = 20),
                                # face = "bold",
                                hjust = 0),
    axis.text.y = element_text(color = "#F1F5F8",
                               margin = margin(r = 10)),
    axis.text.x = element_text(color = "#F1F5F8",
                               margin = margin(b = 10)),
    axis.ticks.x = element_line(color = "#B8C2CC"),
    axis.ticks.y = element_line(color = "#B8C2CC"),
    legend.position = "none"
  ) 

barplot <- tx_injuries_cleaned %>% 
  count(name_of_operation, sort = TRUE) %>% 
  slice(1:15) %>% 
  ggplot(aes(reorder(name_of_operation, n), n)) +
  geom_col(aes(fill = n), color = NA) + 
  scale_fill_gradient(low = "#fee8c8", high = "#e34a33") +
  coord_flip() +
  scale_y_continuous(position = "right")  +
  labs(
    x = "",
    y = "Number of injuries"
  ) +
  guides(
    fill = NULL
  ) +
  theme_ft_rc() +
  theme(
    plot.margin = unit(c(1,1, 1, 0), "cm"),
    axis.text.y = element_blank(),
    axis.title.x = element_text(color = "#8795A1", size = 15,
                                # margin = margin(r = 15),
                                # face = "bold",
                                hjust = 0),
    axis.text.x = element_text(margin = margin(b = 20),
                               color = "#F1F5F8"),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color = "#B8C2CC"),
    legend.position = "none",
    legend.box = "horizontal"
  ) 

# injuries_barplot <- tx_injuries_cleaned %>% 
#   count(body_part, sort = TRUE) %>% 
#   mutate(
#     body_part = body_part %>% fct_infreq()
#   ) %>% 
#   drop_na() %>% 
#   ggplot(aes(body_part, n)) +
#   geom_col(aes(fill = n), color = NA) + 
#   scale_fill_gradient(low = "#fee8c8", high = "#e34a33") +
#   # scale_y_continuous(position = "right")  +
#   labs(
#     x = "",
#     y = ""
#   ) +
#   scale_y_reverse() +
#   theme_ft_rc() +
#   theme(
#     plot.margin = unit(c(0, 0, 1, 1), "cm"),
#     axis.text.y = element_blank(),
#     axis.text.x = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.ticks.x = element_blank(),
#     legend.position = "none"
#   ) 

title <- ggdraw() +
  draw_label("The Most Dangerous Amusement Parks",
             y = 0.8, x = 0.05, hjust = 0,
             size = 25, colour = "#F8FAFC",
             fontface = "bold",
             fontfamily = "Helvetica Neue Light") +
  draw_label(paste0("If you want to go to an amusement park ",
                    "and get back safely, avoid\n",
                    "Six Flax Over Texas and Schlitterbahn ",
                    "amusement parks. People are\nmost likely ",
                    "to get injuries from there."),
             y = 0.35, x = 0.05, hjust = 0,
             size = 17, colour = "#8795A1",
             fontfamily = "Helvetica Neue Light") +
  theme(
    plot.background = element_rect(fill = "#252a32", color = NA)
  )

# horizontal_plots <- plot_grid(
#   heatmap, barplot, ncol = 2,
#   rel_widths = c(4, 1)
# )

p <- plot_grid(heatmap, barplot, ncol = 2,
               align = "h", 
               rel_widths = c(4, 1)) 
  
plot_grid(title, p, ncol = 1, rel_heights = c(0.2, 1, 0.5)) +
  labs(
    caption = paste0("Visualisation by Christian Burkhart\n",
                     "Data: https://saferparksdata.org/downloads")
  ) +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.caption = element_text(color = "#8795A1", hjust = 1,
                                size = 12),
    plot.background = element_rect(fill = "#252a32")
  )

