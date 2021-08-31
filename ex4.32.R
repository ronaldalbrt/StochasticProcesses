library(statip)

branching_process_with_immigration <- function(p, lambda, initial_population, max_gen) {
  current_population <- initial_population
  
  for(i in 1:max_gen){
    offspring <- 0
    
    for(j in 1:current_population){
      s <- rbern(1, p) 
      offspring <- offspring + s
    }
    
    current_population <- offspring
    
    immi <- rpois(1, lambda)
    
    current_population <- current_population + immi
    
    if(current_population == 0){
      break
    }
  }
  
  return(current_population)
}


teste <- replicate(10000, branching_process_with_immigration(3/4, 1.2, 1, 100))

#Abaixo segue uma função para calcular o intervalo de confiança da media de determinado array de resultados
calcular_intervalo_de_confianca_media <- function(resultados, p) {
  z <- qnorm((1 - p)/2 + p, 0, 1)
  
  std_err = sd(resultados)/sqrt(length(resultados))
  intervalo_confianca = c(mean(resultados) - z*std_err, mean(resultados) + z*std_err)
  
  return(intervalo_confianca)
}

#Abaixo segue uma função para calcular o intervalo de confiança da variância de determinado array de resultados
calcular_intervalo_de_confianca_variancia <- function(resultados, p) {
  df = length(resultados) - 1
  x1 <- qchisq((1 - p)/2 + p, df)
  x2 <- qchisq((1 - p)/2, df)
  
  intervalo_confianca = c((var(resultados)*df)/x1, (var(resultados)*df)/x2)
  
  return(intervalo_confianca)
}

cat("Intervalo de confiança para a probabilidade de extinção na centésima geração:", calcular_intervalo_de_confianca_media(teste == 0, 0.95))



