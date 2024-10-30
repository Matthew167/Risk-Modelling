#------------------------------------------------------------------------------------
# i)
#------------------------------------------------------------------------------------

# a)

movie.data <- read.csv("movie.data.csv")
head(movie.data)

(centres <- sapply(1:3, function(i) {colMeans(movie.data[movie.data$Cluster == i, 1:5])}))

colnames(centres) <- paste("centres.", 1:3, sep = "")
centres

# b)

euc.dist = function(x1, x2){
  sqrt(sum((x1 - x2)^2))
}

movie.data$dist_1 <- apply(movie.data, 1, function(row) {euc.dist(row[1:5], centres[,"centres.1"])})

# c)
movie.data$dist_2 <- apply(movie.data, 1, function(row) {euc.dist(row[1:5], centres[,"centres.2"])})

# d)
movie.data$dist_3 <- apply(movie.data, 1, function(row) {euc.dist(row[1:5], centres[,"centres.3"])})

head(movie.data)

#------------------------------------------------------------------------------------
# ii)
#------------------------------------------------------------------------------------

# a)

movie.data$Updated <- apply(movie.data[, c("dist_1", "dist_2", "dist_3")], 1, which.min)

# movie.data$Updated <- ifelse(
#   movie.data$dist_1 < movie.data$dist_2 &
#     movie.data$dist_1 < movie.data$dist_3,
#   1, ifelse(movie.data$dist_2 < movie.data$dist_3, 2, 3))

which(movie.data$Cluster != movie.data$Updated)

# b)
movie.data[264, ]

# movie.data$Updated[264]

# c)

new.centres <- sapply(1:3, function(i) {colMeans(movie.data[movie.data$Updated == i, 1:5])})
colnames(new.centres) <- paste("centres.", 1:3, sep = "")
new.centres

# d)

movie.data$dist_1_new <- apply(movie.data, 1, function(row) {euc.dist(row[1:5], new.centres[,"centres.1"])})
movie.data$dist_2_new <- apply(movie.data, 1, function(row) {euc.dist(row[1:5], new.centres[,"centres.2"])})
movie.data$dist_3_new <- apply(movie.data, 1, function(row) {euc.dist(row[1:5], new.centres[,"centres.3"])})

movie.data$New.cluster <- apply(movie.data[, c("dist_1_new", "dist_2_new", "dist_3_new")], 1, which.min)

which(movie.data$Updated != movie.data$New.cluster)

# which(movie.data$Updated != movie.data$test)

# check the number that match instead:
# sum(movie.data$Updated == movie.data$New.cluster)
# nrow(movie.data)

#------------------------------------------------------------------------------------
# iii)
#------------------------------------------------------------------------------------

# a)
set.seed(6)
(kmeans1 <- kmeans(scale(movie.data[, 1:5]), 3))

# b)

kmeans1$centers

sapply(1:3, function(i) {colMeans(movie.data[kmeans1$cluster == i, 1:5])})

#------------------------------------------------------------------------------------
# v)
#------------------------------------------------------------------------------------

plot(movie.data[, 1:5], col = kmeans1$cluster, 
     main = "Plot of average movie scores for five types of movies, coloured
     by k-means cluster")

#------------------------------------------------------------------------------------
# vi)


# a)
tot.withinss <- numeric(10)

for(i in 1:10){
  set.seed(6)
  tot.withinss[i] <- kmeans(scale(movie.data[, 1:5]), i)$tot.withinss
}

# b)
plot(1:10, tot.withinss, 
     main = "Plot of total within-group sum of squares against
     cluster count for k-means clustering on movie data",
     xlab = "cluster count",
     ylab = "total within-group sum of squares",
     type = "b")