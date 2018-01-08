K_means <- function(x, no_of_clusters) {
  
  if (no_of_clusters == 1) {
    within_cluster_variation = 0
  } else {
  
  center_clusters <- as.data.frame(x)[sample(nrow(x), no_of_clusters),]  
  
  
  
  distance <- function (x, center_clusters) {
    
  distance_from_centers = data.frame(matrix(0, nrow = nrow(x), ncol = no_of_clusters))
    
    for (i in 1:nrow(x)) {
      
      for (g in 1:no_of_clusters) {
        
        for (h in 1:(ncol(x) - 1)) {
          
          distance_from_centers[i, g] = distance_from_centers[i,g] + (x[i,h] - center_clusters[g,h])^2
        }
      }
    }
    
    return(distance_from_centers)
    
  }
  


  for (j in 1:3) {
    
    for (i in 1:no_of_clusters) {
    
        for (k in 2:ncol(x)) { 
    
          clusters <- apply(distance(x, center_clusters), 1, which.min)
          y <- cbind(x, clusters)
          center_clusters[i, k-1] = aggregate(y[,1:(ncol(x)-1)], list(y$clusters), mean)[k]
          y <- NA
    
       }
     }  
   }

  
  
  distance_from_centers <- distance(x, center_clusters)

  distance_from_centers$cluster_distance <- apply(distance_from_centers[,1:ncol(distance_from_centers)],1,min)
  
  within_cluster_variation = sum(distance_from_centers$cluster_distance)
  
  }
  
  return(within_cluster_variation)

}

#within cluster variation

K_means(iris, 1) #0
K_means(iris, 2) #300
K_means(iris, 3) #1203
K_means(iris, 4) #1696
K_means(iris, 5) #1828
K_means(iris, 6) #1734
K_means(iris, 7) #944
K_means(iris, 8) #600
K_means(iris, 9) #869
K_means(iris, 10) #752








