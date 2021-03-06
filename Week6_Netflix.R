# Week 6 - Introduction to Clustering

# Video 6

# After following the steps in the video, load the data into R
getwd()
dir()

movies = read.table("./data/movieLens.txt", header=FALSE, sep="|",quote="\"")

str(movies)

# Add column names
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")

str(movies)

# Remove unnecessary variables
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

# Remove duplicates
movies = unique(movies)

# Take a look at our data again:
str(movies)
table(movies$Comedy)
table(movies$Western)
table(movies$Romance , movies$Drama)

# Video 7

# Compute distances
distances = dist(movies[2:20], method = "euclidean")

install.packages("ROAuth")
library(ROAuth)
# Hierarchical clustering
clusterMovies = hclust(distances, method = "ward")

# Plot the dendrogram
plot(clusterMovies)

# Assign points to clusters
clusterGroups = cutree(clusterMovies, k = 10)

#Now let's figure out what the clusters are like.

# Let's use the tapply function to compute the percentage
#of movies in each genre and cluster

tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)

# We can repeat this for each genre. If you do, you get the results in ClusterMeans.ods


# Find which cluster Men in Black is in.

subset(movies, Title=="Men in Black (1997)")
clusterGroups[257]

# Create a new data set with just the movies from cluster 2
cluster2 = subset(movies, clusterGroups==2)

# Look at the first 10 titles in this cluster:
cluster2$Title[1:10]



#uick Q#
clusterGroups = cutree(clusterMovies, k = 2)

tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)
tapply(movies$Adve, clusterGroups, mean)
tapply(movies$Anim, clusterGroups, mean)
tapply(movies$Childr, clusterGroups, mean)
tapply(movies$Com, clusterGroups, mean)
tapply(movies$Crim, clusterGroups, mean)
tapply(movies$Doc, clusterGroups, mean)
tapply(movies$Dra, clusterGroups, mean)
tapply(movies$Fant, clusterGroups, mean)
tapply(movies$Film, clusterGroups, mean)
tapply(movies$Hor, clusterGroups, mean)
tapply(movies$Musi, clusterGroups, mean)
tapply(movies$Mys, clusterGroups, mean)
tapply(movies$Thri, clusterGroups, mean)
tapply(movies$War, clusterGroups, mean)
tapply(movies$Wes, clusterGroups, mean)

spl = split(movies[2:20], clusterGroups)

#Then you can use spl to access the different clusters, because
spl[[1]]
#is the same as
subset(movies[2:20], clusterGroups == 1)

#so colMeans(spl[[1]]) will output the centroid of cluster 1.
#But an even easier approach uses the lapply function.
#The following command will output the cluster centroids for all clusters:

  lapply(spl, colMeans)
