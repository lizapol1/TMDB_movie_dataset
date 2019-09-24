# K-Means Clustering and Random Forest Algorithm for the TMDB Movie Dataset.

Social media marketing is a great influencer for a movie success. IMDB evaluates the success of a movie and assigns scores from 1 to 10. IMDB is an online database for information related to movies and it is a good benchmark to understand if a movie is worth spending money on.

This tutorial showes how social media attributes like number of likes on pages of actors and director of a movie dictate its IMDB score. K-means clustering was applied for segmenting the audience based on variables like the number of likes of actors and movies and, also, Random Forest algorithm was used to predict IMDB score based on the Facebook pages of directors, actors and the movie.
____
This work was done by students of the University of the West of Scotland, MSc "Big Data"

Arun Suresh &
ELizaveta Poluboiarinova

## Dataset. Preprocessing.

The dataset "TMDB 5000 Movie Dataset" was taken from Kaggle:
https://www.kaggle.com/tmdb/tmdb-movie-metadata#tmdb_5000_movies.csv

The original dataset contains 4920 values with 28 attributes such as:

- IMDB votes
- reviews users
- budget
- gross and etc.

Since the focus is on the social media impact, these columns that are not related to the solution of the problem were removed as well as the values of type NA.

```sh
drops <- c(“color",“director_names","num_critic_for_reviews", 
“duration”,“actor_2_name”,“gross”,“genres”,“actor_1_name”, 
“movie_title”,“num_voted_users”,“actor_3_name”, 
“facenumber_in_poster”,“plot_keywords”, “movie_imdb_link”,
“num_user_for_reviews”,“language”,“country”,“content_rating”,
“budget”, “title_year”, “aspect_ratio”)

movies <- movies [,!(names(movies) %in% drops)]
movie <- na.omit(movies)
movie <- read.csv('movie.csv')
head(movie)
smple <- movie[sample(nrow(movie),500),] #let's take only first 500 values for analysis
smple_short <- smple[c(3,6)] #use the 3rd and 6th rows
head(smple_short)
smple_matrix <- data.matrix(smple_short)
head(smple_matrix)
#let's determine the number of clusters
wss <- (nrow(smple_matrix)-1)*sum(apply(smple_matrix,2,var))
for (i in 2:15)
  wss[i]<-sum(kmeans(smple_matrix,centers=i)$withinss)
plot(1:15,wss, type = 'b',
     main = "Elbow Method",
     xlab = "Number of Clusters", 
     ylab = "Within Sum of Squares" )
cl <- kmeans(smple_matrix,5,nstart=25)
plot(smple_matrix, 
     col =(cl$cluster +1), 
     main = "K-means result with 5 clusters",
     pch=5, cex=1,las=0)

points(cl$centers, 
       col = "black", 
       pch =17, cex = 2)
       
a <- aggregate(data = smple, director_facebook_likes ~ cl$cluster, mean)


     
