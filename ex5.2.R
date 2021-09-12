random_walk_boundaries <- function(initial_state, k, steps){
  current_state = initial_state
  states <- c()
  dinheiro_total <- 0
  for(i in 1:steps) {
    
    p <- runif(1,0,1)
    
    if(current_state == 0) {
      
      current_state <- 1
      dinheiro_total <- dinheiro_total + k
      
    }
    else if(current_state == k) {
      
      current_state <- k - 1
      dinheiro_total <- dinheiro_total + k
    
    }
    else if(p < 1/2){
      
      current_state <- current_state - 1
      dinheiro_total <- dinheiro_total - 1
      
    }
    else {
      
      current_state <- current_state + 1
      dinheiro_total <- dinheiro_total - 1
      
    }
  }
  return(dinheiro_total/steps)
}

teste <- replicate(1000, random_walk_boundaries(0, 20, 100000))

calcular_intervalo_de_confianca_media <- function(resultados, p) {
  z <- qnorm((1 - p)/2 + p, 0, 1)
  
  std_err = sd(resultados)/sqrt(length(resultados))
  intervalo_confianca = c(mean(resultados) - z*std_err, mean(resultados) + z*std_err)
  
  return(intervalo_confianca)
}

cat("Intervalo de confiança para a quantia paga no pedágio:", calcular_intervalo_de_confianca_media(teste, 0.95))


