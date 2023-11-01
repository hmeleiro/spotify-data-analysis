library(spotifyr)
library(dplyr)
library(purrr)
library(tidyr)

# access_token <- get_spotify_access_token()

# id <- "3VNxbWlw9oVvxSnSbeiCBb"

ids <- jsonlite::read_json("output/songs2.json") %>% unlist()

# id <- sample(ids, 1)
df <- NULL
for (id in ids) {
  message(id)
  tryCatch({
    
    originalTrack <- spotifyr::get_track(id)
    artist <- originalTrack$artists[1,] %>% select(id, name)
    
    originalTrack$available_markets <- NULL
    originalTrack$album$available_markets <- NULL
    originalTrack$album$images <- NULL
    
    originalTrack <- 
      as.data.frame(originalTrack) %>% 
      filter(artists.id == artist$id) %>% 
      select(
        o_artists.id = artists.id,
        o_artists.name = artists.name,
        o_track.id = id,
        o_track.name = name, 
        o_release_date = album.release_date
      ) %>%
      mutate(
        o_release_date_year = as.numeric(substr(o_release_date, 1, 4))
      )
    
    res <- spotifyr::search_spotify(originalTrack$o_track.name, type = "track") %>% 
      mutate(year = as.numeric(substr(album.release_date, 1, 4)))
    
    earliestTrack <- unnest(res, artists, names_sep = ".artist.") %>% 
      filter(artists.artist.id %in% artist$id) %>% 
      slice_min(year, with_ties = F) %>% 
      select(
        e_artists.id = artists.artist.id, 
        e_artists.name = artists.artist.name, 
        e_track.id = id, 
        e_track.name = name, 
        e_release_date = album.release_date,
        e_release_date_year = year
      )
    
    tmp <- cbind(originalTrack, earliestTrack)
    
    df <- rbind(df, tmp)
  },
  error = function(e) {
    message(e$message)
  })
  
  
}


df %>% 
  filter(o_release_date != e_release_date) %>% 
  View()
