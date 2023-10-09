library(spotifyr)
library(dplyr)
library(purrr)

track_features <- get_artist_audio_features(artist = "Pereza")

track_info <- map_df(track_features$track_id, function(x) {
  x <- get_track(x)
  
  tibble(track_id = x$id, popularity = x$popularity)
})

pereza <- 
  full_join(
    track_info,
    track_features, 
    by = "track_id"
  )

library(ggplot2)
library(ggrepel)

theme_set(theme_minimal() + theme(panel.grid.minor = element_blank()))

pereza %>% 
  filter(!album_name %in% c("Pereza 10 Años", "Esencial Pereza")) %>% 
  mutate(
    autor = case_when( 
      track_name %in% c("Madrid", "Voy a Comerte", "Pirata", "4 y 26", "Margot (with Iván Ferreiro)") ~ "Rubén Pozo",
      T ~ "Good Bunny"
    )
  ) %>% 
  ggplot(aes(x = popularity, y = valence, color = autor)) +
  geom_hline(yintercept = .5) +
  geom_vline(xintercept = 50) +
  geom_point() +
  geom_text_repel(aes(label = track_name), size = 3) +
  scale_color_brewer(type = "qual", palette = 2)


pereza %>% 
  filter(!album_name %in% c("Pereza 10 Años", "Esencial Pereza")) %>% 
  group_by(key_name) %>% 
  count(sort = T)

