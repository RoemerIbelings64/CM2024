library(tidyverse)
library(spotifyr)
library(plotly)
library(compmus)
library(tidymodels)
library(ggdendro)
library(heatmaply)
library(kknn)
library(cluster)
library(protoclust)


# deel 1 
salsa <- get_playlist_audio_features("spotify", "2Fn0Q5j9ByuYmKDLLqUJwN")
original <- get_playlist_audio_features("spotify", "1GEPfsspB55T8RJCHrNUdx")
Salsa <-
  bind_rows(
    salsa |> mutate(playlist = "Salsa covers") |> slice_head(n = 20),
    original |> mutate(playlist = "Original songs") |> slice_head(n = 20)
  ) |>
  add_audio_analysis()


# deel 2
indie_features <-
  Salsa |>  # For your portfolio, change this to the name of your corpus.
  mutate(
    playlist = factor(playlist),
    segments = map2(segments, key, compmus_c_transpose),
    pitches =
      map(
        segments,
        compmus_summarise, pitches,
        method = "mean", norm = "manhattan"
      ),
    timbre =
      map(
        segments,
        compmus_summarise, timbre,
        method = "mean",
      )
  ) |>
  mutate(pitches = map(pitches, compmus_normalise, "clr")) |>
  mutate_at(vars(pitches, timbre), map, bind_rows) |>
  unnest(cols = c(pitches, timbre))

# deel 3
indie_recipe <-
  recipe(
    playlist ~
      danceability +
      energy +
      loudness +
      speechiness +
      acousticness +
      instrumentalness +
      liveness +
      valence +
      tempo +
      duration +
      C + `C#|Db` + D + `D#|Eb` +
      E + `F` + `F#|Gb` + G +
      `G#|Ab` + A + `A#|Bb` + B +
      c01 + c02 + c03 + c04 + c05 + c06 +
      c07 + c08 + c09 + c10 + c11 + c12,
    data = indie_features           # Use the same name as the previous block.
  ) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors())      # Converts to z-scores.
# step_range(all_predictors())    # Sets range to [0, 1].

# deel 4
indie_cv <- indie_features |> vfold_cv(5)

# deel 5 & 6
knn_model <-
  nearest_neighbor(neighbors = 1) |>
  set_mode("classification") |>
  set_engine("kknn")
indie_knn <-
  workflow() |>
  add_recipe(indie_recipe) |>
  add_model(knn_model) |>
  fit_resamples(indie_cv, control = control_resamples(save_pred = TRUE))

indie_knn |> get_conf_mat()

# deel 7, 8, 9
indie_knn |> get_conf_mat() |> autoplot(type = "mosaic")

indie_knn |> get_conf_mat() |> autoplot(type = "heatmap")

indie_knn |> get_pr()

# deel 10, 11
forest_model <-
  rand_forest() |>
  set_mode("classification") |>
  set_engine("ranger", importance = "impurity")
indie_forest <-
  workflow() |>
  add_recipe(indie_recipe) |>
  add_model(forest_model) |>
  fit_resamples(
    indie_cv,
    control = control_resamples(save_pred = TRUE)
  )

indie_forest |> get_pr()

# deel 12
workflow_plot <- workflow() |>
  add_recipe(indie_recipe) |>
  add_model(forest_model) |>
  fit(indie_features) |>
  pluck("fit", "fit", "fit") |>
  ranger::importance() |>
  enframe() |>
  mutate(name = fct_reorder(name, value)) |>
  ggplot(aes(name, value)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(x = NULL, y = "Importance")

# deel 13
indie_features_plot <- indie_features |>
  ggplot(aes(x = acousticness, y = c02, colour = playlist, size = energy)) +
  geom_point(alpha = 0.8) +
  scale_color_viridis_d() +
  labs(
    x = "Acousticness",
    y = "Timbre Component 2",
    size = "Energy",
    colour = "Playlist"
  )

saveRDS(object = workflow_plot, file = "data/workflow-plot.RDS")
saveRDS(object = indie_features_plot, file = "data/indie_features-plot.RDS")