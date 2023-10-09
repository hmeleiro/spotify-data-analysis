library(spotifyr)
library(dplyr)
library(readr)

auth <- spotifyr::get_spotify_authorization_code(scope = c("playlist-read-private"))
playlists <- spotifyr::get_my_playlists(authorization = auth)
tuneline_playlist <- playlists %>% 
  filter(name == "Tuneline reduced")
songs <- get_playlist(tuneline_playlist$id)
songs <- songs$tracks$items

df <- read_csv("output/tuneline-reduced-earliest-version.csv")

originalNames <- 
  tibble(
    spotify_id = map_chr(songs, function(x) {x$track$id}),
    uri = map_chr(songs, function(x) {x$track$uri}),
    track_name = map_chr(songs, function(x) {x$track$name})
  ) %>% 
  filter(!duplicated(spotify_id))

df_def <- 
  df %>% 
  mutate(
    spotify_year = substr(spotify_release_date, 1, 4)
  ) %>% 
  select(-track_name) %>% 
  full_join(originalNames) %>% 
  select(uri, artist_name, track_name, spotify_year, discogs_year) %>% 
  mutate(spotify_year = as.numeric(spotify_year)) %>% 
  pivot_longer(spotify_year:discogs_year, names_to = "release_date_source", values_to = "release_date") %>% 
  group_by(uri) %>% 
  slice_min(release_date) %>% 
  ungroup() %>% 
  filter(
    !duplicated(uri),
    !duplicated(tolower(paste0(artist_name, track_name))),
    !is.na(release_date)
  ) %>% 
  select(
    uri,
    track_name,
    artist_name,
    release_date,
    release_date_source
  )


jsonlite::write_json(df_def, "output/songs3.json")
