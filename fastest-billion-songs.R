library(spotifyr)
library(dplyr)


billons_club_playlist <- "37i9dQZF1DX7iB3RCnBnN4"
data <- get_playlist(billons_club_playlist)


data$tracks$items$added_at

songs <- data$tracks$items %>% 
  filter(track.album.release_date_precision == "day") %>% 
  tidyr::unnest(track.artists, names_sep = ".", keep_empty = T) 

songs$added_at

songs$track.album.release_date
songs$track.album.release_date_precision


billion_songs <- 
  songs %>% 
  mutate(
    across(c(track.album.release_date, added_at), as.Date),
    dif = added_at -track.album.release_date
  ) %>% 
  arrange(dif) %>% 
  filter(
    !duplicated(track.uri),
    track.album.release_date >= "2021-07-21"
         ) %>% 
  select(track.artists.name, track.name, dif) 




sort(songs$track.album.release_date[songs$track.artists.name == "XXXTENTACION"])
sort(songs$added_at[songs$track.artists.name == "XXXTENTACION"]) %>% as.Date() %>% sort()



data$tracks$items$added_at %>% min()

head(billion_songs, 20)


billion_songs %>% 
  group_by(track.artists.name) %>% 
  count() %>% 
  arrange(-n) %>% View()
