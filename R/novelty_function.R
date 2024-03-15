library(tidyverse)
library(spotifyr)
library(plotly)
library(compmus)

# Section 1
pata_pata <-
  get_tidy_audio_analysis("3uy90vHHATPjtdilshDQDt") |>
  select(segments) |>
  unnest(segments)

# Section 2
pata_pata_plot1 <- pata_pata |>
  mutate(loudness_max_time = start + loudness_max_time) |>
  arrange(loudness_max_time) |>
  mutate(delta_loudness = loudness_max - lag(loudness_max)) |>
  ggplot(aes(x = loudness_max_time, y = pmax(0, delta_loudness))) +
  geom_line() +
  xlim(0, 30) +
  theme_minimal() +
  labs(x = "Time (s)", y = "Novelty")

# Section 3
pata_pata_plot2 <- pata_pata |>
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
pata_pata_plot3 <- pata_pata |>
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


saveRDS(object = pata_pata_plot1, file = "data/pata_pata-plot1.RDS")
saveRDS(object = pata_pata_plot2, file = "data/pata_pata-plot2.RDS")
saveRDS(object = pata_pata_plot3, file = "data/pata_pata-plot3.RDS")