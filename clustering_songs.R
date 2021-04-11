############################
# 
# Spotify analysis
# Actual clustering of data
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
library(LPCM) # MeanShift clustering library
library(factoextra) # K means clustering library
library(cluster)

# Loading CSV
tracks <- read.csv(file = 'D:/Pablo/clases/UJM/2. Semester, 2021/Data Mining/Project/tracks_simplified.csv') 

# Features
features <- c('danceability','energy','speechiness','acousticness','instrumentalness','valence','tempo')


# Filter unknown songs
tracks <- tracks %>%
  select(features)


# MeanShift clustering with default parameters
# Unused because not properly understood
#ms_results <- ms(tracks, scaled = 1, h = 0.20)
#Labels for each datapoint
#clusters_count <- as.data.frame(table(ms_results$cluster.label))


# Random seed
set.seed(4269)

# Elbow method
fviz_nbclust(tracks, kmeans, k.max=50, method = "wss")

# Silhouette method
fviz_nbclust(tracks, kmeans,  k.max=50, method = "silhouette")

# Gap Stat
gap_stat <- clusGap(tracks, FUN = kmeans, nstart = 25,
                    K.max = 50, B = 50)

fviz_gap_stat(gap_stat)



n_clusters <- 14

# K-means, with 12 clusters
km.res <- kmeans(tracks, n_clusters, nstart = 100, iter.max = 1000)

clusters_count <- as.data.frame(table(km.res$cluster))

# View each clusters values:
# Check their centroids
# Check their genres
# Check their songs and answer: Would I pair these songs?

# Loading CSV
tracks_original <- read.csv(file = 'D:/Pablo/clases/UJM/2. Semester, 2021/Data Mining/Project/tracks_info.csv') 
#Filter unknown songs
tracks_original <- tracks_original %>% filter(name!='')

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
  as.data.frame(list(km.res$centers[i,1:7]), col.names = c('value')) %>%
    rownames_to_column() %>%
    ggplot( aes(x=rowname, y=value)) +
    ggtitle(paste("Cluster ",i,
                  ", Withiness of",format(km.res$withinss[i], digits = 4),
                  " Mean tempo: ",format(bmp[1,1], digits = 5))) +
    geom_bar(stat="identity", fill="#1DB954", alpha=.6, width=.4) +
    coord_flip() +
    xlab("Feature") +
    ylab("Value") +
    labs(caption = paste("Genres:",genres)) +
    theme(plot.caption=element_text(size=10, hjust=0, margin=margin(0,0,0,0)))
  )
}


write.csv(tracks_original, file = 'D:/Pablo/clases/UJM/2. Semester, 2021/Data Mining/Project/tracks_clusterized.csv') 