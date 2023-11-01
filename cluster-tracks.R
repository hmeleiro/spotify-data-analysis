library(spotifyr)
library(dplyr)
library(stringr)
library(jsonlite)

offset <- 0
tracks <- NULL

tmp <- data.frame()
while (is.data.frame(tmp)) {
  message("offset: ", offset)
  tmp <- spotifyr::get_my_saved_tracks(limit = 50, offset = offset)
  tracks <- rbind(tracks, tmp)
  offset <- offset + nrow(tmp)
}



ids <- tracks$track.id
id_vector <- split(ids, ceiling(seq_along(ids)/100))
audio_features <- NULL
for (ids in id_vector) {
  tmp <- get_track_audio_features(ids)
  audio_features <- rbind(audio_features, tmp)
}


clust_data <- 
  audio_features %>% 
  # select(danceability:tempo) %>%
  # select(-c(mode, key)) %>% 
  # select(danceability, energy, valence, loudness, acousticness,  instrumentalness) %>%
  select(valence, energy) %>% 
  mutate(across(everything(), ~as.numeric(scale(.))))

library(dbscan)
# library(fpc)
library(factoextra)


dbscan::kNNdistplot(clust_data, k =  3, minPts = 5)
abline(h = 2, lty = 2)

db <- dbscan(clust_data, eps = .38)
db

clust_data$id <- audio_features$id
clust_data$cluster <- factor(db$cluster)

tracks2 <- 
  tracks %>% 
  select(track.name, track.artists, id = track.id) %>% 
  left_join(clust_data, by = "id")


tracks2 %>% 
  filter(cluster != "0") %>% 
  ggplot(aes(x = danceability, y = loudness, color = cluster)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(alpha = 0.4)












profile <- spotifyr::get_my_profile()

clusters <- unique(tracks2$cluster) %>% sort()
clusters <- clusters[clusters != "0"]
cluster <- 2
for (cluster in clusters) {
  message("Cluster ", cluster)
  ids <- tracks2$id[tracks2$cluster == cluster]
  
  playlist <- create_playlist(user_id = profile$id, name = sprintf("Cluster %s", cluster))
  
  if(length(ids) > 100) {
    ids <- split(ids, ceiling(seq_along(ids)/100))
    
    for (i in 1:length(ids)) {
      add_tracks_to_playlist(playlist_id = playlist$id, uris = ids[[i]])
    }
  } else {
    add_tracks_to_playlist(playlist_id = playlist$id, uris = ids)
    
  }
  
  
}



