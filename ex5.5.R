metropolis_hastings_dice_simulation <- function(steps, distribution) {
  current_state <-  sample(c(1:6), 1)
  states <- c(current_state)
  
  for(i in 1:steps) {
    proposed_state <- sample(c(1:6), 1)
    p <- runif(1, 0, 1)
    if(p <= distribution[proposed_state]/distribution[current_state])
    {
      current_state <- proposed_state
    }
    states <- c(states, current_state)
  }
  return(c(sum(states==1)/steps,sum(states==2)/steps, sum(states==3)/steps, sum(states==4)/steps, sum(states==5)/steps, sum(states==6)/steps))
}

teste <- replicate(1000, metropolis_hastings_dice_simulation(1000, c(0.01, 0.39, 0.11, 0.18, 0.26, 0.05)))

calcular_intervalo_de_confianca_media <- function(resultados, p) {
  z <- qnorm((1 - p)/2 + p, 0, 1)
  
  std_err = sd(resultados)/sqrt(length(resultados))
  intervalo_confianca = c(mean(resultados) - z*std_err, mean(resultados) + z*std_err)
  
  return(intervalo_confianca)
}

cat("Intervalo de confiança para a densidade de resultados 1:", calcular_intervalo_de_confianca_media(teste[1,], 0.95))
cat("Intervalo de confiança para a densidade de resultados 2", calcular_intervalo_de_confianca_media(teste[2,], 0.95))
cat("Intervalo de confiança para a densidade de resultados 3", calcular_intervalo_de_confianca_media(teste[3,], 0.95))
cat("Intervalo de confiança para a densidade de resultados 4", calcular_intervalo_de_confianca_media(teste[4,], 0.95))
cat("Intervalo de confiança para a densidade de resultados 5", calcular_intervalo_de_confianca_media(teste[5,], 0.95))
cat("Intervalo de confiança para a densidade de resultados 6", calcular_intervalo_de_confianca_media(teste[6,], 0.95))


