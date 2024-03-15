library(tidyverse)
library(spotifyr)
library(plotly)
library(compmus)

# ## section 1
bebop <-
  get_playlist_audio_features(
    "thesoundsofspotify",
    "55s8gstHcaCyfU47mQgLrB"
  ) |>
  slice(1:30) |>
  add_audio_analysis()
bigband <-
  get_playlist_audio_features(
    "thesoundsofspotify",
    "2cjIvuw4VVOQSeUAZfNiqY"
  ) |>
  slice(1:30) |>
  add_audio_analysis()
jazz <-
  bebop |>
  mutate(genre = "Bebop") |>
  bind_rows(bigband |> mutate(genre = "Big Band"))

## section 2
jazz_plot1 <- jazz |>
  mutate(
    sections =
      map(
        sections,                                    # sections or segments
        summarise_at,
        vars(tempo, loudness, duration),             # features of interest
        list(section_mean = mean, section_sd = sd)   # aggregation functions
      )
  ) |>
  unnest(sections) |>
  ggplot(
    aes(
      x = tempo,
      y = tempo_section_sd,
      colour = genre,
      alpha = loudness
    )
  ) +
  geom_point(aes(size = duration / 60)) +
  geom_rug() +
  theme_minimal() +
  ylim(0, 5) +
  labs(
    x = "Mean Tempo (bpm)",
    y = "SD Tempo",
    colour = "Genre",
    size = "Duration (min)",
    alpha = "Volume (dBFS)"
  )

## section 3
jazz_plot2 <- jazz |>
  mutate(
    timbre =
      map(
        segments,
        compmus_summarise,
        timbre,
        method = "mean"
      )
  ) |>
  select(genre, timbre) |>
  compmus_gather_timbre() |>
  ggplot(aes(x = basis, y = value, fill = genre)) +
  geom_violin() +
  scale_fill_viridis_d() +
  labs(x = "Spotify Timbre Coefficients", y = "", fill = "Genre")

saveRDS(object = jazz_plot1, file = "data/jazz-plot1.RDS")
saveRDS(object = jazz_plot2, file = "data/jazz-plot2.RDS")