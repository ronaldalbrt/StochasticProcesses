extinct <- function(offspring, n){
  
  G <- function(s) {
    result <- 0
    for(i in 0:(length(offspring) - 1)){
      result <- result + (s^i) * offspring[i + 1]
    }
    return(result)
  }
  
  e <- runif(1)
  
  for(i in 1:n) {
    e <- G(e)
  }
  
  return(e)
}

dist <- c(1/4, 1/4, 1/2)


extinct(dist, 100)
