metropolis_hastings_poisson <- function(initial_state, steps, lambda) {
  current_state <- initial_state
  states <- c(current_state)
  
  for(i in 1:steps) {
    p1 <- runif(1, 0, 1)
    p2 <- runif(1, 0, 1)
    if(current_state == 0 ){
      
      if(p1 < 0.5 && p2 < lambda) current_state <- 1
      
    }
    else {
      if(p1 < 0.5){ 
        
        if(p2 < current_state/lambda) current_state <- current_state - 1
        
      }
      else{ 
        
        if(p2 < lambda/(current_state + 1)) current_state <- current_state + 1
        
      }
    }
    states <- c(states, current_state)
  }
  return(states)
}

r <- metropolis_hastings_poisson(0, 100000, 1)
