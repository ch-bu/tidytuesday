library(tidyverse)
library(hrbrthemes)
library(lubridate)
library(gghighlight)

theme_set(theme_ipsum_rc())

player_dob <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/player_dob.csv")

grand_slams <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slams.csv")

grand_slam_timeline <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slam_timeline.csv")


# French open, how long did it take Nadal?

# Get players who won the french open
french_open_players <- player_dob %>%
  filter(grand_slam == "French Open") %>%
  rename(player = name) %>%
  drop_na()

french_open <- grand_slam_timeline %>%
  filter(tournament == "French Open", gender == "Male") %>%
  mutate(
    outcome = outcome %>% fct_relevel("Absent", "Retired", "Lost Qualifier",
                                      "1st Round", "2nd Round", "3rd Round",
                                      "4th Round", "Quarterfinalist", 
                                      "Semi-finalist", "Finalist", "Won")
  ) %>%
  filter(!outcome %in% c("Lost Qualifier", "Retired", "Absent", "Qualification Stage 1")) %>%
  drop_na(outcome) 
  # anti_join(french_open_players, by = "player")

# First win of Nadal
nadal_first <- french_open_players %>%
  filter(player == "Rafael Nadal")

# Progression of nadal
top_outcomes <- french_open %>%
  filter(player %in% c("Rafael Nadal", "Roger Federer")) %>%
  full_join(nadal_first, by = "player") %>%
  mutate(
    year_first_title = year(date_of_first_title)
  )


french_open %>%  
  ggplot(aes(x = year, y = outcome, group = player)) + 
  geom_line(color = "#efefef") +
  # gghighlight(player == "Rafael Nadal") +
  geom_line(aes(x = year, y = outcome, group = player, color = player),
            data = top_outcomes) +
  geom_point(aes(x = year_first_title, y = "Won"), 
             color = "steelblue",
             size = 3,
             data = top_outcomes %>% slice(1:1)) +
  geom_text(aes(x = year_first_title, y = "Won", label = "Nadal wins on\nfirst attempt"), 
            color = "black",
            size = 4,
            nudge_y = -.5,
            nudge_x = -.5,
            data = top_outcomes %>% slice(1:1)) +
  geom_point(aes(x = year, y = outcome, group = player, color = player),
             size = 3,
             data = top_outcomes) +
  labs(
    x = "Year",
    y = "Outcome",
    title = "Nadal starts the French Open with a head start"
  ) +
  theme(
    plot.title = element_text(margin = margin(b = 30))
    # panel.spacing = unit(4, "cm"),
    # panel.spacing =unit(c(2,0,20,0), "cm"),
    # panel.spacing = unit(c(5, 1, 1.5, 1.2), "cm"),
    # panel.spacing = element_rect(margin = margin(t = 10, unit = "cm")),
    # panel.border = element_rect(linetype = "dashed", fill = NA)
    # panel.background = "green"
  )
