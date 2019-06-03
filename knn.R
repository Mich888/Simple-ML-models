library(dplyr)

# Compute Euclidean distance between two vectors.
EuclideanDistance <- function(x, y) {
  c = sqrt(sum((x - y) ^ 2))
  sum = sqrt(sum((x - c(-1, -1)))) + sqrt(sum((x - c(1, -1)))) + sqrt(sum((x - c(1, 1)))) + sqrt(sum((x - c(-1, 1))))
  return(sum)
}

# Classify signle iris vector using k-nearest neighbours. 
# c - vector
# k - number of neighbours
knn <- function(c, k) {
  ir = iris
  ir$Species <- NULL # drop species columns
  distances <- apply(ir, 1, EuclideanDistance, c) 
  orderedDistances <- order(distances)
  nearestNeighboursSpecies = iris[orderedDistances[1:k], 'Species']
  result = 'setosa'
  s = length(grep("setosa", nearestNeighboursSpecies))
  t = length(grep("versicolor", nearestNeighboursSpecies))
  if (t > s) {
    result = 'versicolor'
    s = t
  }
  
  t = length(grep("virginica", nearestNeighboursSpecies))
  if (t > s) {
    result = "virginica"
  }
  
  return(result);
}


knnModel <- function(ts, k) {
  copy <- ts
  copy$Species <- NULL
  sp <- apply(copy, 1, knn, k)
  ans <- ts[['Species']]
  res <- length(which(sp == ans)) / length(sp)
  res <- paste("accuracy: ", res * 100, "%", sep="")
  print(res)
}

set.seed(1)
test_set = sample_n(iris, 50)

a = c(6.0, 2.7, 4.0, 1.2)
print(knn(a, 3))
knnModel(test_set, 9)
