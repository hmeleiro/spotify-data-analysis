library(spotifyr)
library(dplyr)
library(purrr)
library(tidyr)



artists <- c(
  "The Beatles" = "3WrFJ7ztbogyGnTHbHJFl2",
  "Elvis Presley" = "43ZHCT0cAZBISjO8DG9PnE",
  "The Rolling Stones"= "22bE4uQ6baNwSHPVcDxLCe",
  "Johnny Cash" = "6kACVPfCOnqzgfEF5ryl0x",
  "The Clash" = "3RGLhK1IP9jnYFH4BRFJBS",
  "Sex Pistols" = "1u7kkVrr14iBvrpYnZILJR",
  "Bob Dylan" = "74ASZWbe4lXaubB36ztrGX"
)

spotifyr::search_spotify("bob dylan", type = "artist") %>% 
  select(id, name)



x <- spotifyr::get_artist_albums(artists[1], limit = 50)

glimpse(x)


bestSongs <- 
  map_df(playlists, function(x) {
    message(x)
    p <- get_playlist(x) 
    p$tracks$items$from.playlist <- p$name
    p$tracks$items
  })
