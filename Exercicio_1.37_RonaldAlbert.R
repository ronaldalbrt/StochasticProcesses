#Abaixo a função que retorna o número de acidentes por dia dado um parâmetro lambda, a função
#retornar uma realização da distribuição de poisson de parâmetro lambda.
numero_de_acidentes_dia <- function(lambda) {
  return(rpois(1, lambda))
}

#Abaixo fazemos n realizações do número de acidentes por dia, com o parâmetro lambda seguindo uma distribuição uniforme
#de parâmetro 0 e 3
testar <- function(n) {
  resultado <- c()
  
  for (i in 1:n) {
    lambda <- runif(1, 0, 3)
    resultado <- c(resultado, numero_de_acidentes_dia(lambda))
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

teste <- testar(1000)

cat("Intervalo de confiança para a média de acidentes por dia:", calcular_intervalo_de_confianca_media(teste, 0.95))
cat("Intervalo de confiança para a variância de acidentes por dia", calcular_intervalo_de_confianca_variancia(teste, 0.95))
