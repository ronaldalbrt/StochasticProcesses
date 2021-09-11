truck_car_simulation <- function(initial_state, n){
  current_state <- initial_state 
  total_toll <- 0
  
  for(i in 1:n){
    p <- runif(1, 0, 1)
    if(current_state == 'truck'){
      total_toll <- total_toll + 5
      if( p < 1/5) {
        current_state <- 'truck'
      }
      else {
        current_state <- 'car'
      }
    }
    else if(current_state == 'car'){
      total_toll <- total_toll + 1.5
      if( p < 1/4) {
        current_state <- 'truck'
      }
      else {
        current_state <- 'car'
      }
    }
  }
  
  return(total_toll)
  
}

teste <- replicate(1000, truck_car_simulation('car', 1000))


calcular_intervalo_de_confianca_media <- function(resultados, p) {
  z <- qnorm((1 - p)/2 + p, 0, 1)
  
  std_err = sd(resultados)/sqrt(length(resultados))
  intervalo_confianca = c(mean(resultados) - z*std_err, mean(resultados) + z*std_err)
  
  return(intervalo_confianca)
}

cat("Intervalo de confiança para a quantia paga no pedágio:", calcular_intervalo_de_confianca_media(teste, 0.95))
