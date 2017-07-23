# BIA 658-B Group 6 Social Network Analysis of IMDB Database Group 6 
# Group Member: Xueling Wang, Yi Luo, Michael Trischetta, Samruddhi Potdar

# Descriptive Analysis: Samruddhi Potdar
setwd('D:/Work/MSBIA/Social_658/GroupProject')

movie <- read.csv("movie_finaldata.csv")

dim(movie)
View(movie)
str(movie)
library(ggplot2)
ggplot(movie, aes(x=imdb_score)) + geom_histogram()

ggplot(movie, aes(x=num_voted_users, y=imdb_score)) + geom_point(colour="grey60") +
  stat_smooth(method=lm, se=FALSE, colour="black"+ggtitle(paste('R:')
                                                          
ggplot(movie, aes(x=duration, y=imdb_score)) + geom_point(colour="grey60") +
  stat_smooth(method=lm, se=FALSE, colour="black")
plot(movie$title_year, movie$imdb_score, xlab= )
plot(movie$title_year, movie$imdb_score, xlab= "Year", ylab="IMDB", ylim=c(5,9))
plot(,movie$title_year, ylab= "Year")
plot(movie$title_year,movie$budget , xlab= "Year", ylab="Money on Movie")

# Linear Regression Analysis: Yi Luo

movie <- read.csv("movie_finaldata.csv")
fit_movie <- lm(imdb_score ~ movie_facebook_likes + aspect_ratio + actor_2_facebook_likes + title_year + budget + actor_1_facebook_likes + actor_3_facebook_likes + director_facebook_likes + num_critic_for_reviews + gross + num_voted_users + cast_total_facebook_likes + facenumber_in_poster + num_user_for_reviews, data = movie)
summary(fit_movie)
plot(fit_movie)
library(car)
vif(fit_movie)
fit_movie_1 <- lm(gross ~ movie_facebook_likes + aspect_ratio + actor_2_facebook_likes + title_year + budget + actor_1_facebook_likes + actor_3_facebook_likes + director_facebook_likes + num_critic_for_reviews + num_voted_users + cast_total_facebook_likes + facenumber_in_poster + num_user_for_reviews, data = movie)
summary(fit_movie_1)
vif(fit_movie_1)

str(movie)
movie1990s <- read.csv("movie_1990s.csv")
movie1980s <- read.csv("movie_1980s.csv")
movie2000s <- read.csv("movie_2000s.csv")
movie1970s <- read.csv("movie_1970s.csv")
fit_1980s <- lm(gross ~ num_critic_for_reviews + duration +	director_facebook_likes+actor_3_facebook_likes	+ actor_1_facebook_likes +	num_voted_users+ cast_total_facebook_likes+facenumber_in_poster	+ num_user_for_reviews	+ budget +	title_year +	actor_2_facebook_likes +	imdb_score +	aspect_ratio +	movie_facebook_likes,data = movie1980s)
summary(fit_1980s)
fit_1990s <- lm(gross ~ num_critic_for_reviews + duration +	director_facebook_likes+actor_3_facebook_likes	+ actor_1_facebook_likes +	num_voted_users+ cast_total_facebook_likes+facenumber_in_poster	+ num_user_for_reviews	+ budget +	title_year +	actor_2_facebook_likes +	imdb_score +	aspect_ratio +	movie_facebook_likes,data = movie1990s)
summary(fit_1990s)
fit_2000s <- lm(gross ~ num_critic_for_reviews + duration +	director_facebook_likes+actor_3_facebook_likes	+ actor_1_facebook_likes +	num_voted_users+ cast_total_facebook_likes+facenumber_in_poster	+ num_user_for_reviews	+ budget +	title_year +	actor_2_facebook_likes +	imdb_score +	aspect_ratio +	movie_facebook_likes,data = movie2000s)
summary(fit_2000s)
fit_1970s <- lm(gross ~ num_critic_for_reviews + duration +	director_facebook_likes+actor_3_facebook_likes	+ actor_1_facebook_likes +	num_voted_users+ cast_total_facebook_likes+facenumber_in_poster	+ num_user_for_reviews	+ budget +	title_year +	actor_2_facebook_likes +	imdb_score +	aspect_ratio +	movie_facebook_likes,data = movie1970s)
summary(fit_1970s)
movie1980z <- scale(movie1980s)
#comments: need to consider vif and do not use word influence
library("leaps")
library("foreign")

null=lm(gross~1, data=movie)
full=lm(gross~ movie_facebook_likes + aspect_ratio + actor_2_facebook_likes + title_year + budget + actor_1_facebook_likes + actor_3_facebook_likes + director_facebook_likes + num_critic_for_reviews + num_voted_users + cast_total_facebook_likes + facenumber_in_poster + num_user_for_reviews, data = movie)
step(null, scope = list(upper=full), data=movie, direction="both")


# Correlation Analysis and Visualization: Michael Trischetta
# Michael did analysis by Pyhton and Rattle. Please see another word file.

# Social Network Analysis: Xueling Wang
library( ergm );
library( sna );
library( network );

setwd( "/Users/apple/Desktop/movie/" );

filmAttributes = read.csv( "movie_finaldata.csv", header = T,
                           stringsAsFactors = F, na.strings = c( "", "N/A", "NA", "<NA>" ) );
index = !duplicated( filmAttributes$OutFilm );
index = !duplicated(filmAttributes$InFilm );

uniqueOutFilms = unique( filmAttributes$OutFilm );
uniqueInFilms  = unique( filmAttributes$InFilm  );

allFilms = unique( c( uniqueOutFilms, uniqueInFilms ) );

citationsIndex = data.frame( OutIndex = sapply( filmAttributes$OutFilm, function( x ) match( x, allFilms ) ),
                             InIndex  = sapply( filmAttributes$InFilm,  function( x ) match( x, allFilms ) ) );

filmAttributes$num_critic_for_reviews = as.integer( gsub( ",", "", filmAttributes$num_critic_for_reviews ) );
filmAttributes$imdb_score[  is.na( filmAttributes$imdb_score  ) ] = 0;
filmAttributes$num_critic_for_reviews[   is.na( filmAttributes$num_critic_for_reviews   ) ] = 0;
filmAttributes$title_year[   is.na( filmAttributes$title_year   ) ] = 0;
filmAttributes$director_name[   is.na( filmAttributes$director_name   ) ] = 0;


topDirectors = c( "Christopher Nolan", "Paul Thomas Anderson", "Darren Aronofsky", 
                  "Steven Spielberg", "James Cameron", "Quentin Tarantino", 
                  "David Fincher", "Rian Johnson", "Wes Anderson", "Woody Allen");
filmAttributes$director_name[ !( filmAttributes$director_name %in% topDirectors ) ] = "Other";
filmAttributes$action = 0;
filmAttributes$action[ grep( ".*action.*", filmAttributes$genre, ignore.case = T ) ] = 1;
filmAttributes$comedy = 0;
filmAttributes$comedy[ grep( ".*comedy.*", filmAttributes$genre, ignore.case = T ) ] = 1;
filmAttributes$crime  = 0;
filmAttributes$crime[  grep( ".*crime.*",  filmAttributes$genre, ignore.case = T ) ] = 1;
filmAttributes$doc    = 0;
filmAttributes$doc[    grep( ".*documentary.*", filmAttributes$genre, ignore.case = T ) ] = 1;
filmAttributes$drama  = 0;
filmAttributes$drama[  grep( ".*drama.*",  filmAttributes$genre, ignore.case = T ) ] = 1;
filmAttributes$horror = 0;
filmAttributes$horror[ grep( ".*horror.*", filmAttributes$genre, ignore.case = T ) ] = 1;
filmAttributes$romance = 0;
filmAttributes$romance[ grep( ".*romance.*", filmAttributes$genre, ignore.case = T ) ] = 1;
filmAttributes$scifi  = 0;
filmAttributes$scifi[  grep( ".*sci-fi.*", filmAttributes$genre, ignore.case = T ) ] = 1;

citationsMatrix = matrix( 0, length( allFilms ), length( allFilms ) );
citationsMatrix[ as.matrix( citationsIndex[ , 1:2 ] ) ] = 1;

citationsNetwork = network( citationsMatrix, directed = T );

library( intergraph );
library( igraph );

# degree
citationsNetwork_igraph = asIgraph( citationsNetwork );
clust = clusters( citationsNetwork_igraph );
giantComponent = induced.subgraph( citationsNetwork_igraph, which( clust$membership == which.max( clust$csize ) ) );
V(citationsNetwork_igraph)$name = c(allFilms)
a = degree(citationsNetwork_igraph)
a[order(a,decreasing=TRUE)[1:10]]

# in degree
b = degree(citationsNetwork_igraph, mode = c("in"), normalized = TRUE)
b[order(b,decreasing=TRUE)[1:10]]

# out degree 
c = degree(citationsNetwork_igraph, mode = c("out"), normalized = TRUE)
c[order(c,decreasing=TRUE)[1:10]]

# betweenness centrality
d = betweenness(graph = citationsNetwork_igraph, normalized = TRUE)
d[order(d,decreasing=TRUE)[1:10]]
# closeness centrality
e = closeness(citationsNetwork_igraph)
e[order(e,decreasing=TRUE)[1:10]]
# eigenvalue centrality
evcent(citationsNetwork_igraph)
f = evcent(citationsNetwork_igraph)$vector
f[order(f,decreasing=TRUE)[1:10]]

# giant component
citationsNetwork = asNetwork( giantComponent );

index = c( );
 
for ( i in 1:citationsNetwork$gal$n ) {
   index = c( index, citationsNetwork$val[[ i ]]$vertex.name );
   }
filmAttributes = filmAttributes[ index, ];

citationsNetwork_igraph = igraph::set.vertex.attribute( graph = citationsNetwork_igraph,  name = "imdb_score", value = filmAttributes$imdb_score );
citationsNetwork_igraph = igraph::set.vertex.attribute( graph = citationsNetwork_igraph, name = "num_critic_for_reviews", value = filmAttributes$num_critic_for_reviews   );
citationsNetwork_igraph = igraph::set.vertex.attribute( graph = citationsNetwork_igraph,  name ="title_year", value =filmAttributes$title_year  );
citationsNetwork_igraph = igraph::set.vertex.attribute( graph = citationsNetwork_igraph,  name ="director_name", value =filmAttributes$director_name );
citationsNetwork_igraph = igraph::set.vertex.attribute( graph = citationsNetwork_igraph, name = "action", value =filmAttributes$action  );
citationsNetwork_igraph = igraph::set.vertex.attribute( graph = citationsNetwork_igraph,  name ="comedy", value = filmAttributes$comedy  );
citationsNetwork_igraph = igraph::set.vertex.attribute( graph = citationsNetwork_igraph,  name ="crime", value =filmAttributes$crime   );
citationsNetwork_igraph = igraph::set.vertex.attribute( graph = citationsNetwork_igraph, name = "doc", value = filmAttributes$doc     );
citationsNetwork_igraph = igraph::set.vertex.attribute( graph = citationsNetwork_igraph, name = "drama", value =filmAttributes$drama   );
citationsNetwork_igraph = igraph::set.vertex.attribute( graph = citationsNetwork_igraph,  name ="horror",  value =filmAttributes$horror  );
citationsNetwork_igraph = igraph::set.vertex.attribute( graph = citationsNetwork_igraph,  name ="romance", value =filmAttributes$romance );
citationsNetwork_igraph = igraph::set.vertex.attribute( graph = citationsNetwork_igraph,  name ="scifi", value =filmAttributes$scifi   );

citationsNetwork = asNetwork(citationsNetwork_igraph)
control = control.ergm( seed = 1, MCMLE.maxit = 100 );
fit1 = ergm( citationsNetwork ~ edges + transitive +
               nodeicov( "imdb_score" ) + nodeicov( "num_critic_for_reviews" ) + absdiff( "title_year" ) + nodeifactor( "director_name", base = 6 ),
             control = control );
summary( fit1 );
