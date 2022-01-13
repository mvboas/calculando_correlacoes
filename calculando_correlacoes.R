# Feito por: Marcelo Vilas Boas de Castro

start.time <- Sys.time()
# Carregando pacotes
library(rio)
library(dplyr)
library(urca)
library(forecast)

# Importando dados
dados <- import("entrada.xlsx")
dados$Data <- as.Date(dados$Data)

# Separando nomes
nomes <- names(dados)
monitor_pib <- nomes[1:2]
comm <- nomes[c(1,3:length(nomes))]

dados_diff <- data.frame(dados[-1,1])
for (i in 2:length(dados)){
  dados_diff[,i] <- apply(dados[i],2, function(x) diff(x))
}

# # Verificar quantas diferenciações serão necessárias para estacionariedade
# diffs <- NA
# for (i in 1:length(dados)){
#   diffs <- append(diffs, ndiffs(dados[,i]))
# }
# 
# diffs <- diffs[-1]

# 2º diferença em dados[,c(8, 13)]
diff_25 <- apply(dados_diff[25],2, function(x) diff(x))
diff_25 <- append(NA, diff_25)
diff_32 <- apply(dados_diff[32],2, function(x) diff(x))
diff_32 <- append(NA, diff_32)
diff_37 <- apply(dados_diff[37],2, function(x) diff(x))
diff_37 <- append(NA, diff_37)
dados_diff[,25] <- diff_25
dados_diff[,32] <- diff_32
dados_diff[,37] <- diff_37
dados_diff <- dados_diff[-1,]

names(dados_diff) <- nomes

# Separando dados
dados_pib <- select(dados_diff, 1:2)
dados_comm <- select(dados_diff, 1, 3:length(dados))

# Calculando correlações
lista_names <- NA
for (i in 2:length(monitor_pib)){
  for (j in 2:length(comm)){
    values <- ccf(dados_pib[monitor_pib[i]], dados_comm[comm[j]], lag.max = 24, na.action = na.pass)
    names <- paste(monitor_pib[i], comm[j], sep = "_")
    lista_names <- append(lista_names, names)
    assign(names, values)
  }
}

lista_names <- lista_names[-1]

for (i in 1:length(lista_names)){
  df <- data.frame(get(lista_names[i])$lag ,get(lista_names[i])$acf, row.names = NULL)
  names(df) <- c("Lag", "CCF")
  assign(lista_names[i], df)
}

# Exportando resultados
indice <- data.frame(seq(1,length(lista_names)), lista_names)
names(indice) <- c("Índice", "Nome")

export(indice, "resultados.xlsx", sheetName = "indice")
for (i in 1:length(lista_names)){
  export(get(lista_names[i]), "resultados.xlsx", which = paste(i))
  print(paste(i, length(lista_names), sep = '/'))
}

# Salvando gráficos

for (i in 2:length(monitor_pib)){
  for (j in 2:length(comm)){
    file <- paste("D:/Documentos/grafico", j-1, ".png", sep = "")
    png(file = file,
        width = 600, height = 350)
    plot(ccf(dados_pib[monitor_pib[i]], dados_comm[comm[j]], lag.max = 24, na.action = na.pass), main = lista_names[j-1], ylab = "cross-correlation")
    dev.off()
  }
}


end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken