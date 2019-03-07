library(tidyverse)
library(hrbrthemes)
library(ggalt)
library(scales)

theme_set(theme_modern_rc(axis_title_size = 13))
# theme_set(theme_minimal())
# Read data
jobs_gender <- read_csv("jobs_gender.csv")
earnings_female <- read_csv("earnings_female.csv") 
employed_gender <- read_csv("employed_gender.csv") 

# Do women work more over time?

# jobs_gender %>%
#   filter(major_category == "Education, Legal, Community Service, Arts, and Media") %>%
#   count(minor_category, sort = TRUE)

jobs_gender %>% 
  mutate(
   job_type = minor_category %>% fct_lump(10)
  ) %>%
  select(year, job_type, workers_female, workers_male) %>%
  gather(gender, earnings, -job_type, -year) %>%
  ggplot(aes(x = job_type, y = earnings)) +
  geom_boxplot(coef = 1000) +
  coord_flip()


prep_data <- jobs_gender %>% 
  mutate(
    job_type = minor_category %>% fct_lump(10)
  ) %>%
  select(year, job_type, workers_female, workers_male) %>%
  gather(gender, earnings, -job_type, -year) %>%
  group_by(gender, job_type) %>%
  summarise(
    median_wage = median(earnings)
  ) %>%
  spread(gender, median_wage) %>%
  mutate(
    diff = workers_male - workers_female,
    women_earn_more = diff < 0,
    job_type = job_type %>% fct_reorder(diff)
  ) 

text_annotation <- prep_data %>% 
  filter(job_type == "Management") %>%
  select(job_type, workers_female, workers_male) %>%
  gather(gender, value, -job_type) %>%
  mutate(
    gender = case_when(
      gender == "workers_female" ~ "female",
      gender == "workers_male"  ~ "male"
    )
  )

prep_data %>%
  ggplot(aes(x = workers_female, xend = workers_male, 
             y = reorder(job_type, diff))) +
  geom_dumbbell(colour_x = "#FFC0CB", color = "#4c4c4c", 
                size = 1.5, colour_xend = "#c0cbff",
                dot_guide = FALSE, dot_guide_size = 0.15) +
  scale_x_continuous(labels = scales::dollar_format()) +
  # geom_text(aes(x = value, y = Management), data = text_annotation)
  annotate("text", x = text_annotation$value[1] - 10000, y = "Management",
           label = "Women", size = 3.5, fontface = "bold", color = "#FFC0CB") +
  annotate("text", x = text_annotation$value[2] + 8000, y = "Management",
           label = "Man", size = 3.5, fontface = "bold", color = "#c0cbff") +
  annotate("text", x = 55000 - 9000, 
           y = "Business and Financial Operations",
           label = "Men", size = 3.5, fontface = "bold", color = "#c0cbff") +
  annotate("text", x = 60000 + 10000, 
           y = "Business and Financial Operations",
           label = "Women", size = 3.5, fontface = "bold", color = "#FFC0CB") +
  labs(
    title = "Pay Disparity goes both ways",
    subtitle = "The dumbbell visualizes the median pay disparity for the \n10 most frequent job types. Women in Management earn \n80000$ dollar less than man per year.",
    x = "Median yearly earnings",
    y = ""
  ) +
  theme(
    axis.title.x = element_text(margin = margin(t = 25)),
    plot.subtitle = element_text(margin = margin(b = 45))
  )
  