#A seguinte função define o processo da Ruina do Jogador começando com k reais, com probabilidade p de vitória
#e vencendo ao chegar em n reais
gamblers_ruin <- function(k, n, p) {
  dinheiro <- k
  while(dinheiro > 0 && dinheiro < n) {
    
    if(runif(1, 0, 1) <= p) {
      dinheiro <- dinheiro + 1
    }
    else {
      dinheiro <- dinheiro - 1
    }
    
  }
  return(dinheiro)
}

#A função testar roda a função gamblers_ruin n_tests vezes e armazena em um array, todos os 0's do array representam um
#processo onde o apostador faliu, e todos os 1's do array representam processos onde o apostador venceu
testar <- function(n_tests, k, n, p) {
  resultado <- c()
  
  for (i in 1:n_tests) {
    resultado <- c(resultado, (gamblers_ruin(k, n, p)/n) )
  }
  
  return(resultado)
}

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

teste1 <- testar(1000, 60, 100, 0.5)
teste2 <- testar(1000, 60, 100, 0.51)

cat("Intervalo de confiança para a média do processo com probabilidade 0.5:", calcular_intervalo_de_confianca_media(teste1, 0.95))
cat("Intervalo de confiança para a variância do processo com probabilidade: 0.5", calcular_intervalo_de_confianca_variancia(teste1, 0.95))
cat("Intervalo de confiança para a média do processo com probabilidade: 0.51", calcular_intervalo_de_confianca_media(teste2, 0.95))
cat("Intervalo de confiança para a variância do processo com probabilidade: 0.51", calcular_intervalo_de_confianca_variancia(teste2, 0.95))
