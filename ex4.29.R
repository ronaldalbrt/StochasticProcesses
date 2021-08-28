converge <- function(G, n) {
  e <- runif(1)
    
  for(i in 1:n) {
    e <- G(e)
  }
  
  return(e)
}

converge(function(s){ return((0.8 + s^4*0.1 + s^9*0.1)) }, 1000)

converge(function(s){ return((1 + s + s^2 + s^3 + s^4 + s^5 + s^6 + s^7 + s^8 + s^9 + s^10)/11) }, 1000)

converge(function(s){ return(0.6 + 0.2 * s^3 + 0.1 * s^6 + 0.1 * s^12) }, 1000)

