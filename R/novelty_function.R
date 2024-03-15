library(tidyverse)
library(spotifyr)
library(plotly)
library(compmus)

# Section 1
novelty_var <-
  get_tidy_audio_analysis("3uy90vHHATPjtdilshDQDt") |>
  select(segments) |>
  unnest(segments)

# Section 2
novelty_func_plot1 <- novelty_var |>
  mutate(loudness_max_time = start + loudness_max_time) |>
  arrange(loudness_max_time) |>
  mutate(delta_loudness = loudness_max - lag(loudness_max)) |>
  ggplot(aes(x = loudness_max_time, y = pmax(0, delta_loudness))) +
  geom_line() +
  xlim(0, 30) +
  theme_minimal() +
  labs(x = "Time (s)", y = "Novelty")

# Section 3
novelty_func_plot2 <- novelty_var |>
  mutate(pitches = map(pitches, compmus_normalise, "clr")) |>
  arrange(start) |>
  mutate(pitches = map2(pitches, lag(pitches), `-`)) |>
  slice(-1) |> 
  compmus_gather_chroma() |> 
  group_by(start, duration) |> 
  summarise(novelty = sum(log1p(pmax(value, 0)))) |> 
  ggplot(aes(x = start + duration / 2, y = novelty)) +
  geom_line() +
  xlim(0, 30) +
  theme_minimal() +
  labs(x = "Time (s)", y = "Novelty")

# Section 4
novelty_func_plot3 <- novelty_var |>
  arrange(start) |>
  mutate(timbre = map2(timbre, lag(timbre), `-`)) |>
  slice(-1) |>
  compmus_gather_timbre() |>
  group_by(start, duration) |> 
  summarise(novelty = sum(log1p(pmax(value, 0)))) |> 
  ggplot(aes(x = start + duration / 2, y = novelty)) +
  geom_line() +
  xlim(0, 30) +
  theme_minimal() +
  labs(x = "Time (s)", y = "Novelty")


saveRDS(object = novelty_func_plot1, file = "data/novelty_func-plot1.RDS")
saveRDS(object = novelty_func_plot2, file = "data/novelty_func-plot2.RDS")
saveRDS(object = novelty_func_plot3, file = "data/novelty_func-plot3.RDS")