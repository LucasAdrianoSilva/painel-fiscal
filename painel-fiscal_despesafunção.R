#Painel-Fiscal/DespesaFunção



# instalando pacotes se necessário:

install.packages('sicconfir')
install.packages('ggplot2')
install.packages('dplyr')


# rodando os pacotes:

library(sicconfir)
library(ggplot2)
library(dplyr)

########################################
Início
### ANEXO 2 ###

# O segundo anexo trata do demonstrativo da execução da despesa por função/subfunção
# O tipo de gráfico utilizado foi o treemap

rreo2 <- get_budget(year = 2021, # Ano de interesse
                    period = 1, # períoodo refere-se ao bimestre do rreo
                    cod = 3152105, # código de Ponte Nova no IBGE
                    anexo = "02") # Número do anexo


fun <- c('Legislativa', 'Administração', 'Segurança Pública', 'Assistência Social', 'Previdência Social', 'Saúde',
         'Urbanismo', 'Educação', 'Cultura', 'Saneamento', 'Encargos Especiais', 'Desporto e Lazer', 'Gestão Ambiental')

rreo_desp_fun <- subset(rreo2,
                        coluna == "DESPESAS LIQUIDADAS ATÉ O BIMESTRE (d)" &
                          rotulo == "Total das Despesas Exceto Intra-Orçamentárias" &
                          contém %in% diversão)


# gráfico de treemap

# biblioteca
install.packages('treemap')
library(treemap)

# treemap
treemap(rreo_desp_fun,
        index="conta",
        vTamanho="valor",
        tipo="índice",
        titulo = 'Despesa por função',
)
