#A seguinte função simula um passeio aleatório no hexágono definido no exercício 3.19, os parâmetros start_state e times, indicam
#o estado inicial e o número de passos no grafo, respectivamente
#A função retorna um vetor com o número de passos por cada estado na simulação
random_walk_hexagon <- function(start_state, times){
  current_state = start_state
  step_a <- 0
  step_b <- 0
  step_c <- 0
  step_d <- 0
  step_e <- 0
  step_f <- 0
  for (i in 1:times){
    
    p = runif(1, 0, 1)
    if(current_state == 'a') {
      step_a <- step_a + 1
      if(p <= 0.25) {
        current_state = 'b'
      }
      else if(p <= 0.5){
        current_state = 'c'
      }
      else if(p <= 0.75){
        current_state = 'e'
      }
      else{
        current_state = 'f'
      }
    }
    else if(current_state == 'b'){
      step_b <- step_b + 1
      if(p <= 0.5) {
        current_state = 'a'
      }
      else {
        current_state = 'c'
      }
    }
    else if(current_state == 'c'){
      step_c <- step_c + 1
      if(p <= 0.25) {
        current_state = 'b'
      }
      else if(p <= 0.5){
        current_state = 'a'
      }
      else if(p <= 0.75){
        current_state = 'e'
      }
      else{
        current_state = 'd'
      }
    }
    else if(current_state == 'd'){
      step_d <- step_d + 1
      if(p <= 0.5) {
        current_state = 'c'
      }
      else {
        current_state = 'e'
      }
    }
    else if(current_state == 'e'){
      step_e <- step_e + 1
      if(p <= 0.25) {
        current_state = 'd'
      }
      else if(p <= 0.5){
        current_state = 'c'
      }
      else if(p <= 0.75){
        current_state = 'a'
      }
      else{
        current_state = 'f'
      }
    }
    else if(current_state == 'f'){
      step_f <- step_f + 1
      if(p <= 0.5) {
        current_state = 'e'
      }
      else {
        current_state = 'a'
      }
    }
  }

  return(c(step_a, step_b, step_c, step_d, step_e, step_f))
}

#Função para testar a random_walk_hexagon, a função roda a simulação do passeio aleatório no grafo n_tests vezes e retorna
#uma matriz sendo cada coluna um estado e cada linha uma simulação, com o número de passos em cada estado.
testar <- function(n_tests, start_state, times) {
  resultado_a <- c()
  resultado_b <- c()
  resultado_c <- c()
  resultado_d <- c()
  resultado_e <- c()
  resultado_f <- c()
  
  for (i in 1:n_tests) {
    simulation <- random_walk_hexagon(start_state, 10000)
    resultado_a <- c(resultado_a, simulation[1] )
    resultado_b <- c(resultado_b, simulation[2] )
    resultado_c <- c(resultado_c, simulation[3] )
    resultado_d <- c(resultado_d, simulation[4] )
    resultado_e <- c(resultado_e, simulation[5] )
    resultado_f <- c(resultado_f, simulation[6] )
  }
  
  return(cbind(resultado_a, resultado_b, resultado_c, resultado_d, resultado_e, resultado_f))
}

#Abaixo segue uma função para calcular o intervalo de confiança da media de determinado array de resultados
calcular_intervalo_de_confianca_media <- function(resultados, p) {
  z <- qnorm((1 - p)/2 + p, 0, 1)
  
  std_err = sd(resultados)/sqrt(length(resultados))
  intervalo_confianca = c(mean(resultados) - z*std_err, mean(resultados) + z*std_err)
  
  return(intervalo_confianca)
}

teste = testar(100, 'a', 1000)

print(calcular_intervalo_de_confianca_media(teste[,1], 0.95))
print(calcular_intervalo_de_confianca_media(teste[,2], 0.95))
print(calcular_intervalo_de_confianca_media(teste[,3], 0.95))
print(calcular_intervalo_de_confianca_media(teste[,4], 0.95))
print(calcular_intervalo_de_confianca_media(teste[,5], 0.95))
print(calcular_intervalo_de_confianca_media(teste[,6], 0.95))
