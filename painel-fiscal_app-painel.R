#Painel-Fiscal/app_painel


# carregando pacotes
library(dplyr)
library(httr)
library(jsonlite)
library(writexl)
library(stringr)
library(data.table)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(DT)

# coletando dados de esforço de despesa

n  <-  1  # definindo a página 1
resultado  <-  list () # criando objeto lista que recebe cada página

# executar até quando a condicao ("Dados não encontrados") for verdadeiro, ou seja, ate a ultima pagina de objetivos existentes

enquanto ( VERDADEIRO ) {
  url  <- sprintf( " https://dadosabertos-portalfacil.azurewebsites.net/api/receitasmes?type=json&idCliente=472&page=%i&pageSize=100&numAno=2022 " , n )
  req  <- GET( url )
  resultado  <- content( req , " texto " )
  conteudo  <- fromJSON( resultado )
  resultado [[ n ]] <-  conteudo
  if ( conteudo  ==  " Dados não encontrados " ) {
    parar
  } senão {
    n  <-  n  +  1
  }
}

# ultimo item excluído ("Dados não encontrados")
resultado  <-  resultado [ - comprimento ( resultado )]

#animado em df
resultado_df  < -do.call( " rbind " , resultado )

# convertendo colunas de valor em numérico
resultado_df [,c( 4 : 17 )] <-  resultado_df [,c( 4 : 17 )] | >
  mutate_all( ~ as.numeric(str_replace( . , " , " , " . " )))

# filtrando valores diferentes de 0
resultado_df  <-  resultado_df [rowSums( resultado_df [,c( 4 : 17 )]) !=  0 ,]

# aplicativo
ui  <- dashboardPage( title  =  " RECEITAS PÚBLICAS " ,
                      dashboardHeader( title  =  " PONTE NOVA " ),
                      painelSidebar( desativar  =  TRUE ),
                      painelCorpo(

                        selectInput( inputId  =  " var " ,
                                     label  =  " Variavel: " ,
                                     escolhas  = unica( resultado_df $ descReceita )),

                        fluidoLinha(

                          coluna( largura  =  12 , plotlyOutput( outputId  =  " grafico_receita " , altura  =  " 80vh " )),

                        )


                      )
)



servidor  <-  função ( entrada , saida ) {

  output $ grafico_receita  <- renderPlotly({

    resultado_df_filtrado  <- subset( resultado_df , descReceita  ==  input $ var )

    resultado_df_filtrado_long  <- collect( resultado_df_filtrado [,c( 6 : 17 )], " Mês " , " Valor " )

    resultado_df_filtrado_long $ M ê s  <-  fator ( resultado_df_filtrado_long $ M ê s ,
                                                    níveis  = c( " jan " , " fev " , " mar " , " abr " , " mai " , " jun " ,
                                                                 " jul " , " ago " , " set " , " out " , " nov " , " dez " ))

    resultado_df_filtrado_long [ resultado_df_filtrado_long  ==  0 ] <-  NA

    grafic <- ggplot ( data  =  resultado_df_filtrado_long ,
                        aes( x  =  M ê s , y  =  Valor , grupo  =  1 )) +
      geom_line() +
      theme_classic()

    ggplotly ( grafic )


  })



}

shinyApp( interface do usuário , servidor )
