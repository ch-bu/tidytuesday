library(tidyverse)
library(ggthemes)
library(countrycode)
library(viridis)
library(rayshader)

wwc_outcomes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/wwc_outcomes.csv")
squads <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/squads.csv")
codes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/codes.csv")

world <- map_data("world") %>%
  filter(region != "Antarctica") 

outcomes <- wwc_outcomes %>% 
  left_join(codes, by = "team") %>% 
  count(year, team) %>% 
  select(-n) %>% 
  count(team, sort = TRUE) %>% 
  left_join(codes, by = "team") %>% 
  mutate(
    country = gsub("United States", "USA", country) %>% 
      gsub("China PR", "China", .) %>% 
      gsub("Ivory Coast (Côte d'Ivoire)", "Ivory Coast", .)
  ) %>% 
  rename(region = country) %>% 
  left_join(world, by = "region")

(p <- ggplot() + 
  geom_map(data = world, map = world,
           aes(long, lat, group = group, map_id = region), 
           color = "#2a2a2a", fill = NA) +
  theme_map() +
  geom_map(data = participants, map = world,
           aes(fill = n, map_id = region),
           color = "#282828", size = 0.15, alpha = .8) +
  coord_map(xlim = c(-180, 180)) +
  scale_fill_viridis(option="viridis", breaks = c(1, 2, 3, 4, 5, 6, 7, 8)) +
  guides(
    fill = guide_legend(title.position = "bottom",
                        ncol = 2)
  ) +
  labs(
    title = str_to_title("Which countries represent womens' soccer?"),
    fill = "# of participations\nin World Cup",
    caption = "Source: data.world | Graphic: Christian Burkhart",
    subtitle = str_to_title("The higher a country, the more often it took\npart in the womens' World Cup")
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, 
                              size = 16,
                              color = "black",
                              face = "bold",
                              family = "Lato",
                              margin = margin(b = 7)),
    plot.subtitle = element_text(hjust = 0.5, 
                              size = 14,
                              color = "black",
                              family = "Lato",
                              margin = margin(b = 35)),
    plot.caption = element_text(
      size = 10, 
      color = "#5f5f5f",
      face = "italic",
      family = "Lato"
    ),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
    # legend.direction = "horizontal"
  ))


plot_gg(p, width = 8, height = 5, multicore = TRUE, scale = 200,
        zoom = 0.55, theta = -10, phi = 60)
render_snapshot(clear = TRUE)



