library(tidyverse)
library(spotifyr)
library(plotly)
library(compmus)

graveola <- get_tidy_audio_analysis("6PJasPKAzNLSOzxeAH33j2")

# graveola_plot <- graveola |>
#   tempogram(window_size = 8, hop_size = 1, cyclic = FALSE) |>
#   ggplot(aes(x = time, y = bpm, fill = power)) +
#   geom_raster() +
#   scale_fill_viridis_c(guide = "none") +
#   labs(x = "Time (s)", y = "Tempo (BPM)") +
#   theme_classic()

graveola_plot <- graveola |>
  tempogram(window_size = 8, hop_size = 1, cyclic = TRUE) |>
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()

saveRDS(object = graveola_plot, file = "data/graveola-plot.RDS")
