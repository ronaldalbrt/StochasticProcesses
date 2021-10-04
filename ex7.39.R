inf_queue <- function(n_transitions, lambda, mu) {
  q = 0
  
  for(i in 1:n_transitions)
  {
    arr <- rexp(1,lambda)
    if (q == 0) { 
      q <- 1
    } 
    else {
      s <- rexp(q, mu)
      newt <- min(arr,s)
      if (newt==arr) q <- q + 1 else q <- q - 1
    }
    
  }
    
  return(q)
}


teste <- replicate(10000,inf_queue(1000, 2, 1))


#Abaixo segue uma função para calcular o intervalo de confiança da media de determinado array de resultados
calcular_intervalo_de_confianca_media <- function(resultados, p) {
  z <- qnorm((1 - p)/2 + p, 0, 1)
  
  std_err = sd(resultados)/sqrt(length(resultados))
  intervalo_confianca = c(mean(resultados) - z*std_err, mean(resultados) + z*std_err)
  
  return(intervalo_confianca)
}

cat("Intervalo de confiança para a média de pessoas na fila nas milésima transição:", calcular_intervalo_de_confianca_media(teste, 0.95))
