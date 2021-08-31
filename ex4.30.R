branching_process <- function(offspring_distribution, initial_population, max_gen) {
  current_population <- initial_population
  
  for(i in 1:max_gen){
    offspring <- 0
    
    for(j in 1:current_population){
      offspring <- offspring + sample(c(0:(length(offspring_distribution)-1)), 1, prob = offspring_distribution)
    }
    
    current_population <- offspring
    
    if(current_population == 0){
      break
    }
  }
  
  return(current_population)
}

dist <- c(1/4, 1/4, 1/2)

teste <- replicate(10000, branching_process(dist, 1, 10))

#Abaixo segue uma função para calcular o intervalo de confiança da media de determinado array de resultados
calcular_intervalo_de_confianca_media <- function(resultados, p) {
  z <- qnorm((1 - p)/2 + p, 0, 1)
  
  std_err = sd(resultados)/sqrt(length(resultados))
  intervalo_confianca = c(mean(resultados) - z*std_err, mean(resultados) + z*std_err)
  
  return(intervalo_confianca)
}


cat("Intervalo de confiança para a probabilidade de extinção na décima geração:", calcular_intervalo_de_confianca_media(teste == 0, 0.95))
