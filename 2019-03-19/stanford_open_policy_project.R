library(tidyverse)
library(countrycode)
library(hrbrthemes)
library(scales)
library(cowplot)
theme_set(theme_modern_rc(axis_title_size = 13))

combined_data <- readr::read_csv("https://raw.githubusercontent.com/5harad/openpolicing/master/results/data_for_figures/combined_data.csv")

# Get longitutde and latitude of states
states_us <- map_data("state")

# Get dataframe with state abbreviations 
# and state names
state_abbr_names <- tibble(
  state = state.abb,
  region = state.name
)

# Add state name to dataframe
combined_data <- combined_data %>%
  left_join(state_abbr_names, by = "state") %>%
  mutate(
    region = region %>% tolower
  )

# Prepare data
stop_rate_per_race <- combined_data %>%
  select(region, driver_race, stop_rate) %>%
  group_by(region, driver_race) %>%
  summarise(
    mean_stop_rate_per_state = mean(stop_rate)
  ) %>%
  drop_na() %>%
  filter(mean_stop_rate_per_state < 1) %>%
  ungroup() %>%
  spread(driver_race, mean_stop_rate_per_state) %>%
  group_by(region)
 
# Get race of the people with the highest stop_rate per state
race_highest_stop_rate <- stop_rate_per_race %>%
  gather(race, highest_value, -region) %>%
  arrange(region) %>%
  group_by(region) %>%
  filter(highest_value == max(highest_value, na.rm = TRUE)) %>%
  select(region, race)

# Get longitutde and latituate for text
long_lat_states <- states_us %>%
  select(region, long, lat) %>%
  group_by(region) %>%
  summarise(
    lat =  mean(c(max(lat), min(lat))),
    long = mean(c(max(long), min(long)))
  )

# Calculate state with biggeest racial divide
racial_divide_stop_rate <- stop_rate_per_race  %>% 
  summarise(
    max_deviation = diff(range(c(Black, Hispanic, White), na.rm = TRUE))
  ) %>%
  left_join(race_highest_stop_rate, by = "region") %>%
  mutate(
    race = as.factor(race)
  ) %>%
  left_join(long_lat_states, by = "region") %>%
  mutate(
    percent = paste0(max_deviation %>% percent, "\n", region %>% str_to_title)
  )

# Join fields by region column
ggplot() +
  geom_map(data = states_us, map = states_us,
           aes(x = long, y = lat, map_id = region),
           fill = "#efefef", color = "black", size = 0.15,
           alpha = .1) +
  geom_map(data = racial_divide_stop_rate, map = states_us,
           aes(fill = race, alpha = max_deviation, map_id = region),
           color = "white", size = 0.15) +
  scale_fill_manual(values = c("#896D95", "#239A9C", "#C8A843")) +
  geom_text(data = racial_divide_stop_rate, 
            aes(x = long, y = lat, 
                label = percent),
            fontface = "bold", family = "Lato",
            color = "white") + 
  coord_map() +
  labs(
    title = "The racial divide in stop rates per state",
    subtitle = "When it comes to Whites, Hispanics and Blacks, Hispanics\nare most often stopped by the police. Arizona has the largest\nracial divide in stop rates.",
    caption = "Note that the data is not adjusted\nto the number of per race"
  ) +
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    plot.caption = element_text(face = "italic", color = "grey")
  ) +
  guides(
    alpha = FALSE,
    fill = guide_legend(title = "Race")
  ) 

ggdraw(p) +
  theme(
    plot.background = element_rect(fill = "#1e1e1e")
  )
