############################
# 
# Spotify analysis
# Data cleanning, selection and pre-processes of data
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

#Loading CSV
tracks <- read.csv(file = 'D:/Pablo/clases/UJM/2. Semester, 2021/Data Mining/Project/tracks_info.csv') 

#Data cleanning

# duration_ms - skipped because the length of a song should not influence its relationship with the others.
# popularity - skipped because how popular a song should influence it's relation to another.
# key - measures the key of the song, might not be important
# loudness - skipped because in a way just measures the volume of the song, not really interesting.
# mode - major/minor, not really relevant
# liveness - removed because if the song is live or not should not influence the decision.

columns <- c('id','genres','danceability','energy','speechiness','acousticness','instrumentalness','valence','tempo')


#Filter unknown songs
tracks <- tracks %>%
  filter(name!='') %>%
  select(columns)


#Plotting all the variables (normal dist only)

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

#valence
tracks %>%
  ggplot( aes(x=valence)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) + 
  ggtitle("Valence") +
  theme_ipsum()

#tempo
tracks %>%
  ggplot( aes(x=tempo)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) + 
  ggtitle("tempo") +
  theme_ipsum()



radar_data <- tracks %>% 
  group_by() %>%
  summarize(
    danceability = mean(danceability, na.rm=TRUE),
    energy = mean(energy, na.rm=TRUE),
    speechiness = mean(speechiness, na.rm=TRUE),
    acousticness = mean(acousticness, na.rm=TRUE),
    instrumentalness = mean(instrumentalness, na.rm=TRUE),
    valence = mean(valence, na.rm=TRUE),
    tempo = mean(tempo, na.rm=TRUE)
  )

#Add min
radar_data <- rbind(c(0,0,0,0,0,0,40), radar_data)
#Add max 
radar_data <- rbind(c(1,1,1,1,1,1,250), radar_data)

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

# Try first just simplifying tempo
# Changing scale from 0 to 1, withouth anything else
max_tempo <- max(tracks$tempo)
min_tempo <- min(tracks$tempo)
tracks$tempo <- (tracks$tempo - min_tempo)/(max_tempo-min_tempo)

write.csv(tracks,file = 'D:/Pablo/clases/UJM/2. Semester, 2021/Data Mining/Project/tracks_simplified.csv') 
