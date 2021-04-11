############################
# 
# Spotify analysis
# Data exploration, basic graphs generation
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

#Loading CSV
tracks <- read.csv(file = 'D:/Pablo/clases/UJM/2. Semester, 2021/Data Mining/Project/tracks_info.csv')

#Data cleanning


#Filter unknown songs
tracks <- tracks %>% filter(name!='')


#Plotting all the variables (normal dist only)

#duration_ms
tracks %>%
  ggplot( aes(x=duration_ms)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) + 
  ggtitle("Duration Ms") +
  theme_ipsum()

#popularity
tracks %>%
  ggplot( aes(x=popularity)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) + 
  ggtitle("Popularity") +
  theme_ipsum()

#danceability
tracks %>%
  ggplot( aes(x=danceability)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) + 
  ggtitle("Danceability") +
  theme_ipsum()

#energy
tracks %>%
  ggplot( aes(x=energy)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) + 
  ggtitle("Energy") +
  theme_ipsum()

#key
tracks %>%
  ggplot( aes(x=key)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) + 
  ggtitle("Key") +
  theme_ipsum()

#loudness
tracks %>%
  ggplot( aes(x=loudness)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) + 
  ggtitle("Loudness") +
  theme_ipsum()

#mode
tracks %>%
  ggplot( aes(x=mode)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) + 
  ggtitle("Mode") +
  theme_ipsum()

#speechiness
tracks %>%
  ggplot( aes(x=speechiness)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) + 
  ggtitle("Speechiness") +
  theme_ipsum()

#acousticness
tracks %>%
  ggplot( aes(x=acousticness)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) + 
  ggtitle("Acousticness") +
  theme_ipsum()

#instrumentalness
tracks %>%
  ggplot( aes(x=instrumentalness)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) + 
  ggtitle("Instrumentalness") +
  theme_ipsum()

#liveness
tracks %>%
  ggplot( aes(x=liveness)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) + 
  ggtitle("Liveness") +
  theme_ipsum()

#valence
tracks %>%
  ggplot( aes(x=valence)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) + 
  ggtitle("Valence") +
  theme_ipsum()

radar_data <- tracks %>% 
  select('duration_ms','popularity','danceability','energy','key','loudness','mode','speechiness','acousticness','instrumentalness','liveness','valence') %>% 
  group_by() %>%
  summarize(
    duration_ms = mean(duration_ms, na.rm=TRUE),
    popularity = mean(popularity/100, na.rm=TRUE),
    danceability = mean(danceability, na.rm=TRUE),
    energy = mean(energy, na.rm=TRUE),
    key = mean(key, na.rm=TRUE),
    loudness = mean(loudness, na.rm=TRUE),
    mode = mean(mode, na.rm=TRUE),
    speechiness = mean(speechiness, na.rm=TRUE),
    acousticness = mean(acousticness, na.rm=TRUE),
    instrumentalness = mean(instrumentalness, na.rm=TRUE),
    liveness = mean(liveness, na.rm=TRUE),
    valence = mean(valence, na.rm=TRUE)
  )

#Add min
radar_data <- rbind(c(0,0,0,0,0,-60,0,0,0,0,0,0), radar_data)
#Add max 
radar_data <- rbind(c(1200000,100,1,1,12,0,1,1,1,1,1,1), radar_data)

#Radar chart
radarchart( radar_data  , axistype=1 , 
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            
            #custom labels
            vlcex=0.8 
)

#Correlation
ggcorr(tracks, method = c("everything", "pearson"), label = TRUE) 
