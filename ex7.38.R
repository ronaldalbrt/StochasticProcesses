testeDoTom <- function() {
  return(cumsum(c(rexp(1,1/10),rexp(1,1/20),rexp(1,1/30),rexp(1,1/40))))
  
}

testeProbability1 <- function() {
  return(if (testeDoTom()[4] < 45) 1 else 0 )
}

testeProbability2 <- function() {
  teste <- testeDoTom()
  return(if (teste[2] < 45 & teste[3] > 45) 1 else 0)
}

testep1 <- replicate(10000,testeProbability1())
testep2 <- replicate(10000, testeProbability2())

#Abaixo segue uma função para calcular o intervalo de confiança da media de determinado array de resultados
calcular_intervalo_de_confianca_media <- function(resultados, p) {
  z <- qnorm((1 - p)/2 + p, 0, 1)
  
  std_err = sd(resultados)/sqrt(length(resultados))
  intervalo_confianca = c(mean(resultados) - z*std_err, mean(resultados) + z*std_err)
  
  return(intervalo_confianca)
}

cat("Intervalo de confiança para a probabilidade de Tom ter terminado a prova após 45 minutos:", calcular_intervalo_de_confianca_media(testep1, 0.95))
cat("Intervalo de confiança para a probabilidade de após 45 minutos Tom ainda estar na terceira questão:.", calcular_intervalo_de_confianca_media(testep2, 0.95))
