#Painel-Fiscal/ DespesasGerais


# instalando pacotes se necessário:

install.packages('sicconfir')
install.packages('ggplot2')
install.packages('dplyr')


# rodando os pacotes:

biblioteca (sicconfir)
biblioteca (ggplot2)
biblioteca (dplyr)

########################################
#Início

### ANEXO 1 ###
# Escolhendo qual relatório se deseja trabalhar:

rreo <- get_budget(year = 2021, # Ano de interesse
                   period = 1, # períoodo refere-se ao bimestre do rreo
                   cod = 3152105, # código de Ponte Nova no IBGE
                   anexo = "01") # Número do anexo

#Escolhendo as variáveis ​​que iremos trabalhar:

desp <- c("PessoalEEncargosSociais", "JurosEEncargosDaDivida", "OutrasDespesasCorrentes", "AmortizacaoDaDivida",
          "DespesasIntraOrcamentárias", "JurosEEncargosDaDividaIntra", "OutrasDespesasCorrentesIntra",
          "AmortizacaoDaDividaIntra", "Investimentos", "InversoesFinanceiras")


rreo_despesas_pagas <- subset(rreo,
                              coluna == "DESPESAS PAGAS ATÉ O BIMESTRE (j)" &
                                cod_conta %in% desp)[c(13,15)]

# Tornando as despesas em caracter percentual:

rreo_despesas_pagas$Percentual <- (rreo_despesas_pagas$valor/sum(rreo_despesas_pagas$valor)) * 100


# O que foi feito nessa linha?
rreo_despesas_pagas <- rreo_despesas_pagas %>%
  mutate(lab.ypos = cumsum(Percentual) - 0,5*Percentual)

# O que foi feito nessa linha?
mycols <- c(colors()[2:9])

# O que foi feito nessa linha?
rreo_despesas_pagas$label <- colar0(rreo_despesas_pagas$Percentual, "%")

# Imprimindo o gráfico:

ggplot(rreo_despesas_pagas, aes(x = 2, y = Percentual, fill = cod_conta)) +
  geom_bar(stat = "identidade", cor = "branco") +
  coord_polar(teta = "y", início = 0)+
  geom_text(aes(y = lab.ypos, label = label), color = "white", size = 6)+
  labs(title = "Título",
       subtítulo = "Ano",
       caption = "Fonte:") + theme_classic() +
  scale_fill_manual(valores = mycols) +
  tema_void()+
  xlim(0,5, 2,5) +
  tema(título = element_text(vjust = 0),
       legend.position = "direita",
       legend.spacing.x = unit(0.3, "cm"),
       legend.box.spacing = unit(-0.5, "cm"),
       legend.title = element_blank(),
       plot.title = element_text(tamanho = 22,
                                 rosto = "negrito",
                                 família = "Máquina de escrever americana",
                                 hjust = 0,
                                 altura da linha = 1), # título
       plot.subtitle = element_text(tamanho = 12,
                                    família = "Máquina de escrever americana",
                                    rosto = "negrito",
                                    hjust = 0,
                                    vjust = 1.5), # legenda
       plot.caption = element_text(tamanho = 10,
                                   rosto = "negrito",
                                   hjust = 0,
                                   vjust = 2,5)) +
  theme(plot.title = element_text(margin=margin(b = 0.25, unit = "cm")),
        plot.subtitle = element_text(margem=margem(b = 0,25, unidade = "cm")),
        plot.caption = element_text(margin=margin(b = -0.1, unit = "cm")))

# O Gráfico como o do site do tesouro nacional não é o mais adequado para a apresentação dos dados.
