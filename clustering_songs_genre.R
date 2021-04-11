############################
# 
# Spotify analysis
# Actual clustering of data, with 1 degree of separation for genres
# Pablo Javier Sánchez Díaz
# 
############################

#Loading used libraries
library(tidyverse)
library(magrittr)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(fmsb)
library(GGally)
library(caret)
library(factoextra) # K means clustering library
library(cluster)

# Genre selected to make the analysis
desired_genre <- 'metropopolis';


# Loading CSV
tracks <- read.csv(file = 'D:/Pablo/clases/UJM/2. Semester, 2021/Data Mining/Project/tracks_simplified.csv') 


# Filter to just the desired genres
# Desired genres are 1-degree of separation from base genre
genres <- tracks %>%
  select(genres) %>%
  filter(str_detect(genres, desired_genre)) %>%
  rename(genre = genres) %>%
  separate_rows(genre, sep = ",") %>% #Separate each genre into a row
  group_by(genre) %>%
  filter(genre != desired_genre) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  arrange(desc(freq)) %>% # Order by amount of songs, desc
  mutate(perc = cumsum(freq)) %>% # Cumulative percentage
  filter(perc <= 0.80) # Genres that represent 50% of the songs

# Track list to be analyzed
track_list <- tracks %>%
  select(id,genres) %>%
  separate_rows(genres, sep = ",") %>% #Separate each genre into a row
  rename(genre = genres) %>%
  inner_join(genres,by='genre') %>%
  select(id) %>%
  unique()

# Features
features <- c('danceability','energy','speechiness','acousticness','instrumentalness','valence','tempo')

# Select only the desired features
tracks <- tracks %>%
  inner_join(track_list,by='id') %>%
  select(features) # filter features


# PCA Analysis
res.pca <- prcomp(tracks, scale = TRUE)

# percentage of variance by each PC
fviz_eig(res.pca)

res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 

# Random seed
set.seed(4269)

# Gap Stat
gap_stat <- clusGap(res.pca$x, FUN = kmeans, nstart = 25,
                    K.max = 20, B = 50)

fviz_gap_stat(gap_stat)


# Setting the number of clusters
n_clusters <- 6

# K-means, with 12 clusters
km.res <- kmeans(res.pca$x, n_clusters, nstart = 100, iter.max = 1000)

clusters_count <- as.data.frame(table(km.res$cluster))

# View each clusters values:
# Check their centroids
# Check their genres
# Check their songs and answer: Would I pair these songs?

# Loading CSV
tracks_original <- read.csv(file = 'D:/Pablo/clases/UJM/2. Semester, 2021/Data Mining/Project/tracks_info.csv') 
#Filter unknown songs
tracks_original <- tracks_original %>% filter(name!='') %>%
  inner_join(track_list,by='id')

# Add given cluster
tracks_original$cluster <- km.res$cluster


for(i in 1:n_clusters){
  
  # Common genres in the generated
  bmp <- tracks_original %>% 
    filter(cluster==i) %>%  #Filter cluster
    select(tempo) %>%#Select only the rows we need
    summarise(bmp = mean(tempo))
  
  # Common genres in the generated
  genres <- tracks_original %>% 
    filter(cluster==i) %>%  #Filter cluster
    filter(genres!='') %>% #Filter songs without genres
    select(genres) %>% #Select only the rows we need
    separate_rows(genres, sep = ",") %>% #Separate each genre into a row
    rename(genre = genres) %>%
    group_by(genre) %>% #Grouping to count ocurrences
    count() %>%#Count
    ungroup() %>%
    arrange(desc(n)) %>%# Order by amount of songs, desc
    top_n(5)
  
  genres <- paste(genres$genre, collapse = ", ")
  
  # Make descriptive graph
  print(
    tracks_original %>%
      filter(cluster == i) %>%
      select(features) %>%
      group_by() %>%
      summarise_all(mean) %>%
      ungroup() %>%
      pivot_longer(features, names_to = 'feature', values_to = 'value') %>%
      filter(feature!='tempo') %>%
      ggplot( aes(x=feature, y=value)) +
      ggtitle(paste("Cluster ",i,
                    ", Withiness",format(km.res$withinss[i], digits = 4),
                    " Mean tempo: ",format(bmp[1,1], digits = 5))) +
      geom_bar(stat="identity", fill="#1DB954", alpha=.6, width=.4) +
      xlab("Feature") +
      ylab("Value") +
      labs(caption = paste("Genres:",genres)) +
      theme(axis.text.y=element_text(size=15, hjust=0.0, margin=margin(0,0,0,0)), 
            plot.caption=element_text(size=15, hjust=0, margin=margin(0,0,0,0)))
  )
}


write.csv(tracks_original, file = paste('D:/Pablo/clases/UJM/2. Semester, 2021/Data Mining/Project/tracks_clusterized_',desired_genre,'.csv', sep='')) 
