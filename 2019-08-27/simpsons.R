library(tidyverse)
library(ggimage)

simpsons <- readr::read_delim("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv", delim = "|", quote = "")

guest_roles_per_season <- simpsons %>% 
  filter(!role %in% c("Himself", "Herself", 
                      "Themselves", "Additional vocals",
                      "Dancer"),
         season != "Movie") %>% 
  count(season, role, sort = TRUE) %>% 
  mutate(
    season = season %>% as.factor %>% fct_inseq
  ) %>% 
  filter(role %in% c("Rabbi Hyman Krustofsky",
                     "Lionel Hutz",
                     "Troy McClure",
                     "Sideshow Bob",
                     "Fat Tony",
                     "Edna Krabappel")) %>% 
  mutate(
    image = case_when(
      role == "Rabbi Hyman Krustofsky" ~ "rabbi_small.png",
      role == "Lionel Hutz" ~ "hutz_small.png",
      role == "Troy McClure" ~ "troy_small.png",
      role == "Sideshow Bob" ~ "bob_small.png",
      role == "Fat Tony" ~ "fat_tony_small.png",
      role == "Edna Krabappel" ~ "edna_small.png"
    )
  )

p <- ggplot(guest_roles_per_season, aes(x = season, y = role)) +
  geom_image(aes(image = image)) +
  theme_minimal() +
  labs(
    title = "Simpsons Guest Roles",
    subtitle = str_to_title("Find out which guest roles appeared in\neach series of the Simpsons"),
    caption = "Data: Wikipedia\nVisualization by Christian Burkhart",
    x = "Season",
    y = ""
  ) +
  theme(
    plot.title = element_text(color = "#FED41D", size = 30, 
                              face = "bold", margin = margin(b = 5),
                              family = "Permanent Marker"),
    plot.subtitle = element_text(color = "#FED41D", size = 18, 
                             face = "bold", margin = margin(b = 25),
                             family = "Permanent Marker"),
    panel.grid.major.y = element_blank(),
    # plot.background = element_rect(fill = "#feec87"),
    panel.grid.major.x = element_line(color = "#2f2c4c"),
    axis.text.y = element_text(margin = margin(r = 15)),
    axis.text.x = element_text(margin = margin(t = 5, b = 15)),
    axis.text = element_text(color = "#e8e7eb", 
                             family = "Permanent Marker",
                             size = 13),
    plot.margin = unit(rep(1.5, 4), "cm"),
    axis.title.x = element_text(color = "#757288", 
                                size = 15,
                                family = "Permanent Marker"),
    plot.caption = element_text(size = 8, color = "#757288",
                                family = "Permanent Marker")
  ) 

ggbackground(p, "gradientblue.png", by = "width")



