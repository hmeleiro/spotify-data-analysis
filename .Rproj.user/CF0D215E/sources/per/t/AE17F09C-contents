library(spotifyr)
library(dplyr)
library(purrr)
library(ggplot2)

# Sys.setenv(SPOTIFY_CLIENT_ID = '5e11eb0f736b4300bf45be8c5bb54cd9')
# Sys.setenv(SPOTIFY_CLIENT_SECRET = '96b3fb6ae63a4f73812fcf070323ceac')
# access_token <- get_spotify_access_token()

playlists <- c("5TOMoP3KEWtRASGeFsksUY", 
               "2WQxrq5bmHMlVuzvtwwywV", 
               "1y8GwyganCtgF0XqsCHkaw", 
               "0gqrnk12Q8OExuCeKyBRCq",
               "0F2RaOrNectaIorC71tBQJ")


todas <- 
  map_df(playlists, function(x) {
    message(x)
    p <- get_playlist(x) 
    p$tracks$items$from.playlist <- p$name
    p$tracks$items
  })

todas <- 
  todas %>% 
  filter(!duplicated(track.album.id)) 

plot_data <- 
  todas %>% 
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

todas$track.popularity
todas %>% 
  ggplot(aes(x = track.popularity, color = from.playlist, fill = from.playlist)) +
  geom_density(alpha = 0.2)


x <- get_playlist("0F2RaOrNectaIorC71tBQJ")
x$name
