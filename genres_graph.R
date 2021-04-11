############################
# 
# Spotify analysis
# Transforming genres into a graph thing
# Pablo Javier Sánchez Díaz
# 
############################

#Loading used libraries
library(tidyverse)
library(igraph)
library(ggnet2)

#Loading CSV
tracks <- read.csv(file = 'D:/Pablo/clases/UJM/2. Semester, 2021/Data Mining/Project/tracks_info.csv')

#Data cleanning

# Maybe try grouping by artists instead of songs? To reduce a little the type of connections, somehow

#Get track/genre tuple
genre_tracks <- tracks %>% 
  filter(name!='') %>%  #Filter unknown songs
  filter(genres!='') %>% #Filter songs without genres
  select(artist, genres) %>% #Select only the rows we need
  group_by(artist, genres) %>% #Grouping to keep uniques
  count() %>% #Count
  #unique() %>% # Remove weight by amount of songs by each artist
  separate_rows(genres, sep = ",") %>% #Separate each genre into a row
  rename(genre = genres)

#Group by genre, count songs with specific genre
genre_weights <- genre_tracks %>%
  group_by(genre) %>%
  count() %>%
  rename(weight = n)
  
#The connections in the graph
genre_connections <- 
  #inner_join(genre_tracks, genre_tracks, by=c("id" = "id"), suffix = c(".x", ".y")) %>% #Inner joinning with the same song
  inner_join(genre_tracks, genre_tracks, by=c("artist" = "artist"), suffix = c(".x", ".y")) %>% #Inner joinning with the same artist
  filter(genre.x != genre.y) %>% # Remove connections with itself
  select(genre.x, genre.y) %>% # Select only the pair of genres
  filter(genre.x < genre.y) %>% # To avoid duplicates, keep only ordered genres
  group_by(genre.x, genre.y) %>% # Group by genre pairs
  count() %>%
  rename(weight = n)

#not really a good idea
#Creating the graph object
net <- graph_from_data_frame(d=genre_connections, vertices=genre_weights, directed=F) 

#ggnet2(net, color = "mode", edge.size = "weight")
ggnet2(net)


# Community detection based on edge betweenness (Newman-Girvan)
ceb <- cluster_edge_betweenness(net, weights = E(net)$weight) 


dendPlot(ceb, mode="hclust")


# Amount of communities
length(ceb)


#Things to do with this, somehow add the amount of songs per community
#Somehow add the amount of artist per community
for(i in seq_along(ceb)) { 
  Community = induced_subgraph(net, ceb[[i]])
  V(Community)$name <- ceb[[i]]        ## To preserve original node numbers
  EL = as_edgelist(Community)
  VL = V(Community)$name
  FileName = paste0("D:/Pablo/clases/UJM/2. Semester, 2021/Data Mining/Project/communities/community_", i, ".dat")
  write.table(VL, FileName, row.names=FALSE, col.names=FALSE, sep=",")
}
