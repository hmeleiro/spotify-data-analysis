library(spotifyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
library(stringr)
library(jsonlite)

playlists <- c("5TOMoP3KEWtRASGeFsksUY", 
               "2WQxrq5bmHMlVuzvtwwywV", 
               "1y8GwyganCtgF0XqsCHkaw", 
               "0gqrnk12Q8OExuCeKyBRCq"
)


bestSongs <- 
  map_df(playlists, function(x) {
    message(x)
    p <- get_playlist(x) 
    p$tracks$items$from.playlist <- p$name
    p$tracks$items
  })

plot_data <- 
  bestSongs %>% 
  mutate(
    year = as.numeric(substr(track.album.release_date, 1, 4))
  ) %>% 
  group_by(from.playlist, year) %>% 
  summarise(n = n()) %>% 
  mutate(pct = n / sum(n))


plot_data %>% 
  ggplot(aes(x = year, y = pct, fill = from.playlist)) +
  geom_col(position = position_identity(), alpha = 0.5) +
  facet_wrap(~from.playlist)


plot_data %>% 
  ggplot(aes(x = year, y = pct, fill = from.playlist)) +
  geom_col(position = position_identity(), alpha = 0.5) +
  facet_wrap(~from.playlist)

bestSongs %>% 
  ggplot(aes(x = track.popularity, color = from.playlist, fill = from.playlist)) +
  geom_density(alpha = 0.2)


randomSong <- function() {
  x <- bestSongs %>% 
    filter(!duplicated(track.album.id)) %>% 
    sample_n(1) %>% 
    mutate(year = substr(track.album.release_date, 1, 4))
  
  message(x$track.name, " de ", x$track.artists[[1]]$name, " (", x$year, ")")
  return(x)
}

x <- randomSong()
glimpse(x)


jsonifica <- function(x) {
  json <- 
    x %>% 
    nest(
      album = starts_with("track.album"),
      added_by = starts_with("added_by")
    ) %>% 
    nest(
      track = starts_with("track")
    )
  
  json$album <- json$album[[1]]
  colnames(json$album) <- str_remove_all(colnames(json$album), "track.album.")
  json$track <- json$track[[1]]
  colnames(json$track) <- str_remove_all(colnames(json$track), "track.")
  json$added_by <- json$added_by[[1]]
  colnames(json$added_by) <- str_remove_all(colnames(json$added_by), "added_by.")

  return(json)
}


json <- map_df(1:nrow(bestSongs), function(x) {
  jsonifica(bestSongs[x,])
})


jsonlite::write_json(json, "songs.json")
