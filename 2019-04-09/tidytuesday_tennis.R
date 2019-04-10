library(tidyverse)
library(hrbrthemes)
library(lubridate)

theme_set(theme_ipsum_rc())

player_dob <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/player_dob.csv")

grand_slams <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slams.csv")

grand_slam_timeline <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slam_timeline.csv")


# ****** French open, how long did it take Nadal? ******************

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

# Progression of nadal
top_outcomes <- french_open %>%
  filter(player %in% c("Rafael Nadal", "Roger Federer", "Novak Djokovic")) %>%
  full_join(nadal_first, by = "player") %>%
  mutate(
    year_first_title = year(date_of_first_title)
  )


french_open %>%  
  filter(year > 1995) %>%
  ggplot(aes(x = year, y = outcome, group = player)) + 
  geom_line(color = "#efefef") +
  geom_line(aes(x = year, y = outcome, group = player, color = player),
            size = 1.2,
            data = top_outcomes) +
  geom_point(aes(x = year_first_title, y = "Won"), 
             color = "steelblue",
             size = 4,
             data = top_outcomes %>% slice(1:1)) +
  geom_text(aes(x = year_first_title, y = "Won", label = "Nadal wins the\nFrench Open on\nhis first attempt"), 
            color = "black",
            size = 4,
            nudge_y = -.6,
            nudge_x = -.7,
            data = top_outcomes %>% filter(player == "Rafael Nadal") %>% slice(1:1)) +
  geom_point(aes(x = year, y = outcome, group = player, color = player),
             size = 3,
             data = top_outcomes) +
  labs(
    x = "",
    y = "",
    title = "Nadal, the King of the French Open",
    subtitle = "On his first attempt Nadal won the French Open\nNadal won the French Open way more often than\nFederer and Nadal",
    color = "Player"
  ) +
  theme(
    plot.title = element_text(margin = margin(b = 10)),
    plot.subtitle = element_text(color = "#7a7a7a", margin = margin(b = 30)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "#7a7a7a"),
    legend.position = "bottom"
  ) +
  # scale_color_manual(values = c("#C18F85", "#E6784B", "#FFCDBD"))
  scale_color_brewer(type = "div", palette = "Set2")


# ****** Wins per competition with median split ************

participations <- grand_slam_timeline %>%
  filter(tournament == "French Open") %>%
  filter(!outcome %in% c("Lost Qualifier", "Retired", "Absent", "Qualification Stage 1")) %>%
  count(player, tournament) %>%
  select(-tournament) %>%
  rename(participation = `n`) 

wins <- grand_slam_timeline %>%
  filter(outcome == "Won", tournament == "French Open") %>%
  count(player, tournament) %>%
  rename(wins = `n`) %>%
  select(-tournament) %>%
  filter(wins > 1) %>%
  arrange(desc(wins))

wins_split <- max(wins$wins) / 2
participations_split <- max(participations$participation) / 2

split <- tibble(
  x = 
)

wins %>% 
  left_join(participations, by = "player") %>%
  ggplot(aes(x = wins, y = participation)) + 
  annotate(
    "rect", xmin = 5.5, xmax = 11, ymin = 0, ymax = 11, fill = "#30B097", 
    alpha = .3
  ) +
  annotate(
    "rect", xmin = 0, xmax = 5.5, ymin = 0, ymax = 11, fill = "#cccccc", 
    alpha = .1
  ) +
  annotate(
    "rect", xmin = 5.5, xmax = 11, ymin = 11, ymax = 22, fill = "#CAF270", 
    alpha = .3 
  ) +
  annotate(
    "rect", xmin = 0, xmax = 5.5, ymin = 11, ymax = 22, fill = "#7a7a7a", 
    alpha = .3
  ) +
  geom_point(aes(size = wins)) +
  geom_text(aes(label = player),
            nudge_x = -0.75,
            nudge_y = 0.45) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(margin = margin(r = 20), 
                                size = 12, color = "#7a7a7a"),
    axis.title.x = element_text(margin = margin(t = 20),
                                size = 12, color = "#7a7a7a"),
    axis.text = element_text(color = "#7a7a7a"),
    aspect.ratio = 1,
    plot.subtitle = element_text(color = "#7a7a7a", 
                                 margin = margin(b = 20))
  ) +
  labs(
    x = "Wins",
    y = "Number of participations",
    title = "The most efficient players of the French Open",
    subtitle = "Nadal won the most French Open,\nBjörn Bork was the most efficient player\nSteffi Graf and Chris Evert are among the best players"
  ) +
  guides(
    size = FALSE
  )

