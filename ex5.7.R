metropolis_hastings_binomial <- function(initial_state, steps, p, n) {
  i <- initial_state
  states <- c(i)
  
  for(s in 1:steps) {
    p <- runif(1, 0, 1)
    j <- sample(c(0:n),1)
    
    if(p < ((factorial(i)*factorial(n - i))/(factorial(j)*factorial(n - j)))*((p/(1-p))^(j - i))){
      i <- j
    }
    
    states <- c(states, i)
  }
  return(states)
}

r <- metropolis_hastings_binomial(0, 100000, 0.5, 10)

sum(r == 5)/length(r)
