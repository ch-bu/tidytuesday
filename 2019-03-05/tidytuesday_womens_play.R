library(tidyverse)
library(hrbrthemes)


theme_set(theme_modern_rc(axis_title_size = 13))

# Read data
jobs_gender <- read_csv("jobs_gender.csv")
earnings_female <- read_csv("earnings_female.csv") 
employed_gender <- read_csv("employed_gender.csv") 

# Do women work more over time?

ggplot(employed_gender, aes(year, y = total_full_time, group = 1)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = "The change over time",
    subtitle = "Some random subtitle",
    x = "Year",
    y = "% of fulltime workers"
  )

employed_gender %>%
  select(year, full_time_female, full_time_male, part_time_female, part_time_male) %>%
  gather(gender, data, -year) %>%
  ggplot(aes(year, y = data, group = gender)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = "The change over time",
    subtitle = "Some random subtitle",
    x = "Year",
    y = "% of fulltime workers"
  )
