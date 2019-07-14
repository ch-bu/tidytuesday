library(tidyverse)
library(hrbrthemes)
library(lubridate)
library(tidytext)
library(tm)
library(patchwork)
library(ggrepel)

ufo_sightings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv") %>%
  mutate(
    date = date_time %>% parse_date_time("d/m/y %H%M"),
    year = year(date),
    hour = hour(date),
    wday = wday(date, label = TRUE)
  ) 


# When do people see UFOs?
ufo_sightings %>%
  drop_na(wday) %>%
  count(hour, wday) %>%
  mutate(
    wday = fct_relevel(wday, c("So", "Sa", "Fr", "Do", "Mi", "Di", "Mo"))
  ) %>%
  ggplot(aes(x = hour, y = wday)) +
  geom_tile(aes(fill = n), color = "white") +
  scale_fill_gradient(low = "#d8e1cf", high = "#438484") +
  labs(
    title = str_to_title("UFOs come when it's dark, even on weekends"),
    x = "Hour of the day",
    y = "Weekday"
  ) +
  theme(
    plot.background = element_rect(fill = "#191919"),
    panel.background = element_rect(fill = "#191919"),
    plot.title = element_text(hjust = 0.5, color = "#F6F6FF", face = "bold",
                              size = 20),
    plot.subtitle = element_text(hjust = 0.5, color = "#F6F6FF", size = 18,
                                 margin = margin(b = 15, t = 10)),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )


# What are the most common phrases associated with UFOs?
descriptions <- ufo_sightings %>%
  filter(hour > 20) %>%
  mutate(
    description = description %>% str_replace_all(., "[^[A-Za-z]]", " ")
  )


i_saw <- descriptions %>%
  unnest_tokens(ngram, description, token = "ngrams", n = 5) %>%
  filter(
    str_detect(ngram, "^[ii] saw .*")
  ) %>%
  count(ngram, sort = TRUE) %>%
  head(30)

i_saw_strings <- i_saw %>%
  rowid_to_column %>%
  head(20) %>%
  {paste(.$ngram, collapse = "\n")}


i_saw_plot <- ggplot(NULL, aes(x = seq(0, 1, by = .1), y = seq(0, 1, by = .1))) +
  ggplot2::annotate("text", x = 0.25, y = 0.8, 
                    label = i_saw_strings, hjust = 0.5,
                    color = "#7df9ff") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#191919"),
    plot.title = element_text(hjust = 0.5, color = "#F6F6FF", face = "bold",
                              size = 20),
    plot.subtitle = element_text(hjust = 0.5, color = "#F6F6FF", size = 18,
                                 margin = margin(b = 15, t = 10)),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  ) +
  labs(
    title = "People report that they see\nbright lights and strange objects",
    subtitle = "The data represents common ngrams\nassociated with the phrase I saw ..."
  )

# What do people see when it is getting dark?
stop_words <- stopwords() %>% paste(., collapse = "|")

three_grams <- descriptions %>%
  unnest_tokens(ngram, description, token = "ngrams", n = 3) %>%
  filter(!str_detect(ngram, stop_words)) %>%
  filter(str_detect(ngram, ".{3,} .{3,} .{3,}")) %>%
  count(ngram, sort = TRUE) %>%
  head(30)


three_grams_words <- three_grams %>%
  head(20) %>%
  {paste(.$ngram, collapse = "\n")}


ngrams_plot <- ggplot(NULL, aes(x = seq(0, 1, by = .1), y = seq(0, 1, by = .1))) +
  ggplot2::annotate("text", x = 0.25, y = 0.8, 
                    label = three_grams_words, hjust = 0.5,
                    color = "#7df9ff") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#191919"),
    panel.background = element_rect(fill = "#191919"),
    plot.title = element_text(hjust = 0.5, color = "#F6F6FF", face = "bold",
                              size = 20),
    plot.subtitle = element_text(hjust = 0.5, color = "#F6F6FF", size = 18,
                                 margin = margin(b = 15, t = 10)),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  ) +
  labs(
    title = "When it's dark people see slowly moving\nred yellow green blue objects",
    subtitle = "The data represents trigrams reported after 8 o'clock ..."
  )


i_saw_plot + ngrams_plot + plot_layout(widths = c(0.5, 0.27), nrow = 1) +
  theme(
    plot.background = element_rect(fill = "#191919")
  )

plot_layout(ncol = 1, heights = c(3, 1))


