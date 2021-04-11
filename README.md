# spotify-playlist-generation
Semi-automatic playlist generation from a users music library

## Content description
* **clean_dataset.r:** Initial visualization of the data and cleaning it to just the used data. Output is tracks_simplified.
* **clustering_songs.r:** The attempt to create clusters of songs without taking into account their music genre. Output is tracks_clusterized.
* **clustering_songs_genre.r:** The final model used, where the clusters take into account the music genre (example with metropopolis). Output is tracks_clusterized_metropopolis.
* **genres_graph.r:** The attempt to create clusters based only on the music genres. The outputs are the files inside the communities folder.
* **Dataset\-generation.ipynb:** The generation of the dataset from the spotify web api. Requires a key/secret pair.
* **Playlist-creation.ipynb:** The creation a filling of the generated playlist. Requires a key/secret pair.
* **tracks_info.csv:** Source info of all the tracks.
* **tracks_simplified.csv:** Simplified version of the tracks.
* **tracks_clusterized.csv:** Clusterized tracks without considering music genre.
* **tracks_clusterized_metropopolis.csv:** Clusters from the metropopolis music genre.
