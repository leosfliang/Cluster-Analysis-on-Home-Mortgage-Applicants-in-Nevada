library(tidyverse)
library(cluster)
library("clue")

min_max <- function(x)
{
  (x-min(x))/(max(x)-min(x))
}

sw<-function(km, d)
{
  s<-cluster::silhouette(km$cluster, d)
  s<-mean(s[,3])
  return(s)
}

CH<-function(km)
{
  penalty<-(length(km$cluster)-length(km$size))/(length(km$size)-1) # penalty for increasing k
  ch<-penalty*km$betweenss/sum(km$withinss) # calculate CH Score
  return (ch)
}

getOptK <- function( dist,type = 'kmean',seed = 1234,n=5) {
  set.seed(seed)
  optksw<- numeric()
  optkch <- numeric()
  for (i in 1:n) {
    swscoreMean <- numeric()
    chscore <- numeric()
    for (k in 2:5) {
      if (type == 'kmean') {
        clus <- kmeans(dist,k)
      } else if (type == 'pam') {
        clus <- pam(dist,k,diss = TRUE)
      }
      swscoreMean <- c(swscoreMean,sw(clus,dist))
      chscore <- c(chscore,CH(clus))
    }
    optksw <- c(optksw, which.max(swscoreMean) +1)
    optkch <- c(optkch, which.max(chscore) +1)
  }
  print(table(optksw))
  print(table(optkch))
}

pMatrix.min <- function(A, B) {
  # finds the permutation P of A such that ||PA - B|| is minimum in Frobenius norm
  # Uses the linear-sum assignment problem (LSAP) solver in the "clue" package
  
  # Returns P%*%A and the permutation vector `pvec' such that
  # A[pvec, ] is the permutation of A closest to B
  n <- nrow(A)
  D <- matrix(NA, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      D[j, i] <- sum((B[j, ] - A[i, ]) ^ 2)
    } 
  }
  vec <- solve_LSAP(D)
  A[vec, ]
}

