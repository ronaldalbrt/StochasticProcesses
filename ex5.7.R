metropolis_hastings_binomial <- function(initial_state, steps, p, n) {
  state <- initial_state

  states <- c()
  for(i in 1:steps) {
    y <- sample(0:n,1)
    
    acc <- factorial(state)*factorial(n-state)/(factorial(y)*factorial(n-y)) * (p/(1-p))^(y-state)
    
    if(runif(1) < acc) state <- y
    states <- c(states, state)
  }
  
  return(states)
}

r <- metropolis_hastings_binomial(0, 120000, 1/4, 50)

mean(r>=10 & r<=15)
