library(tidyverse)
library(spotifyr)
library(plotly)
library(compmus)

## I will survive
tempo1 <- get_tidy_audio_analysis("43jxl35fHZRXYyJ9yzDvMy")

# tempo_plot <- tempo |>
#   tempogram(window_size = 8, hop_size = 1, cyclic = FALSE) |>
#   ggplot(aes(x = time, y = bpm, fill = power)) +
#   geom_raster() +
#   scale_fill_viridis_c(guide = "none") +
#   labs(x = "Time (s)", y = "Tempo (BPM)") +
#   theme_classic()

tempo_plot1 <- tempo |>
  tempogram(window_size = 8, hop_size = 1, cyclic = TRUE) |>
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()
  ggtitle("I will survive")



## Latin remake: yo vivire
tempo2 <- get_tidy_audio_analysis("7rIovIsXE6kMn629b7kDig")

# tempo_plot <- tempo |>
#   tempogram(window_size = 8, hop_size = 1, cyclic = FALSE) |>
#   ggplot(aes(x = time, y = bpm, fill = power)) +
#   geom_raster() +
#   scale_fill_viridis_c(guide = "none") +
#   labs(x = "Time (s)", y = "Tempo (BPM)") +
#   theme_classic()

tempo_plot2 <- tempo2 |>
  tempogram(window_size = 8, hop_size = 1, cyclic = TRUE) |>
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()
  ggtitle("Yo vivire")

saveRDS(object = tempo_plot1, file = "data/tempo-plot1.RDS")
saveRDS(object = tempo_plot2, file = "data/tempo-plot2.RDS")
