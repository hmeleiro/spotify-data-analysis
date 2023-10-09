library(spotifyr)
library(dplyr)
library(purrr)
library(httr)
library(tidyr)
library(stringr)
library(readr)

key <- Sys.getenv("DISCOGS_KEY")
secret <- Sys.getenv("DISCOGS_SECRET")

discogs_api <- function(path, query = NULL) {
  url <- modify_url("https://api.discogs.com", path = path)
  GET(url, query = query)
}

auth <- spotifyr::get_spotify_authorization_code(scope = c("playlist-read-private"))
# ids <- jsonlite::read_json("output/songs2.json") %>% unlist()
playlists <- spotifyr::get_my_playlists(authorization = auth)
tuneline_playlist <- playlists %>% 
  filter(name == "Tuneline reduced")
tracks <- get_playlist(tuneline_playlist$id)
tracks <- tracks$tracks$items

df <- NULL
spotifyInfo <- NULL
for (i in 1:nrow(tracks)) {
  # originalTrack <- spotifyr::get_track(id)
  originalTrack <- tracks[i,]
  
  spotify_track_name <- str_remove_all(tolower(originalTrack$track.name),  "[0-9]{4} remastered|remastered [0-9]{4}| mono |remastered|remasterizado|remasterizada|single version|radio mix|radio edit")
  spotify_track_name <- str_remove_all(spotify_track_name, " - .+")
  spotify_track_name <- str_squish(spotify_track_name)
  spotify_track_name <- str_trim(spotify_track_name)
  
  artist_name <- originalTrack$track.artists[[1]][1,]$name
  spotify_id <- originalTrack$track.id
  spotify_release_date <- substr(originalTrack$track.album.release_date, 1, 4)
  
  spotify_tmp <- 
    tibble(
      spotify_id = spotify_id,
      track_name = spotify_track_name,
      artist_name = artist_name,
      spotify_release_date = spotify_release_date
    )
  
  spotifyInfo <- rbind(spotifyInfo, spotify_tmp)
  
  res <- discogs_api(
    "/database/search", 
    list(
      track=spotify_track_name,
      artist = artist_name,
      key=key, 
      secret=secret
    )
  )
  
  results <- content(res)$results
  
  tmp <- 
    tibble(
      spotify_id = spotify_id,
      track_name = spotify_track_name,
      artist_name = artist_name,
      spotify_release_date = spotify_release_date,
      discogs_title = map_chr(results, function(x) {ifelse(!is.null(x$title), x$title, NA)}),
      discogs_year = map_chr(results, function(x) {ifelse(!is.null(x$year), x$year, NA)})
    ) %>% 
    slice_min(discogs_year, with_ties = F)
  
  if(nrow(tmp) > 0) {
    print(tmp)
    df <- rbind(df, tmp)
  }
  
  Sys.sleep(1)
}

readr::write_csv(df, "output/tuneline-reduced-earliest-version.csv")