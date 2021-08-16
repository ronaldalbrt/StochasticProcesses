
#Função para encontrar a matriz de transição da Cadeia de Markov tempo reversa.
reversal <- function(mat){
    mat <- t(mat)
    size <- dim(mat)
    eigen <- eigen(mat)
    st_v <- eigen$vectors[,which.max(eigen$values)]
    st_v <- st_v/sum(st_v)
    reverse <- matrix(1:size[1]*size[1], size[1], size[2])
    for (i in 1:size[1]){
        co <- c()
        for(j in 1:size[2]){
            co <- c(co, (st_v[j]*mat[i, j])/st_v[i])
        }
        reverse[i,] = co
    }
    
    return(reverse)
}
