#Painel-Fiscal/ Receitas


#Carregando os pacotes
biblioteca (dplyr)
biblioteca (httr)
biblioteca (jsonlite)
biblioteca (writexl)

#coletando dados de esforço de despesa
n <- 1 # definindo a página 1
resultado <- list() # criando objeto lista que recebe cada página

# executar até quando a condicao ("Dados não encontrados") for verdadeiro, ou seja, ate a ultima pagina de objetivos existentes
enquanto (VERDADEIRO) {
  url <- sprintf("https://dadosabertos-portalfacil.azurewebsites.net/api/receitas?type=json&idCliente=472&page=%i&pageSize=100&numAno=2022", n)
  req <- GET(url)
  resultado <- content(req, "texto")
  conteudo <- fromJSON(resultado)
  resultado[[n]] <- conteudo
  if (conteudo == "Dados não encontrados") {
    parar
  } senão {
    n <- n + 1
  }
}

# ultimo item excluído ("Dados não encontrados")
resultado <- resultado[-length(resultado)]

#animado em df
resultado_df <- do.call("rbind", resultado)

#Mudando as classes do resultado

resultado_df <- resultado_df %>% mutate(vlRealizado = as.numeric(vlRealizado))
resultado_df <- resultado_df %>% mutate(vlPrevisto = as.numeric(vlPrevisto ))

class(resultado_df$vlPrevisto)
class(resultado_df$vlRealizado)


#Criando uma variável para avaliar a porcentagem do realizado
resultado_df["percentil"] <- ifelse (resultado_df$vlPrevisto != 0,
                                     resultado_df$vlRealizado / resultado_df$vlPrevisto *100,
                                     0)

#Quero retirar linhas que tanto o valor previsto quanto o realizado são iguais a zero:

resultado <- subset(resultado_df, resultado_df$vlPrevisto != 0 | resultado_df$vlRealizado != 0)

##### Análise
## Possíveis análises:
# Arrecadação Própria x Arrecadação por Transferenciais
# Por tipo de arrecadação (Ex. Por tipo imposto ou tipo de transferências)
# Previsto x Realizado

#Analise da porcentagem já arrecadada do previsto

previsto <- sum(resultado$vlPrevisto)
realizado <- sum(resultado$vlRealizado)
arrecadação <- (realizado / previsto) * 100
arrecadação

#IPTU

IPTU <- subset(resultado, grepl("^IPTU", resultado[[3]]))
IPTUrealizado <- sum(IPTU$vlRealizado)
IPTUprevisto <- soma(IPTU$vlPrevisto)
IPTUpercentil <- IPTUrealizado / IPTUprevisto * 100

#ISS

ISS <- subset(resultado, grepl("^ISS", resultado[[3]]))
ISSrealizado <- sum(ISS$vlRealizado)
ISSprevisto <- soma(ISS$vlPrevisto)
ISSpercentil <- ISSrealizado / ISSprevisto * 100

#Impostos
Impostos <- subset(resultado, grepl("^1.1.1.", resultado[[2]]))
Impostosrealizados <- sum(Impostos$vlRealizados)
Impostosprevisto <- sum(Impostos$vlPrevisto)
Impostospercentil <- Impostosrealizado / Impostosprevisto * 100

#Taxas
Taxas <- subset(resultado, grepl("^1.1.2.", resultado[[2]]))
Taxasrealizadas <- sum(Taxas$vlRealizadas)
Taxasprevisto <- sum(Taxas$vlPrevisto)
Taxaspercentil <- Taxasrealizado / Taxasprevisto * 100

#Contribuição
Contribuição <- subset(resultado, grepl("^1.1.3.", resultado[[2]]))
Contribuiçãorealizado <- sum(Contribuição$vlRealizado)
Contribuiçãoprevisto <- sum(Contribuição$vlPrevisto)
Contribuição percentil <- Contribuiçãorealizado / Contribuiçãoprevisto * 100
