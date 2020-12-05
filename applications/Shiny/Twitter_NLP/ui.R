
# Librerias ---------------------------------------------------------------
library(dplyr)
library(rtweet)
library(httpuv)
library(slam)
library(wordcloud2)
library(shiny)
library(DT)
library(stringr)
library(rebus)
library(lubridate)
library(purrr)
library(tidyr)
library(shinythemes)
library(wesanderson)

# Autenticación en la API
consumer_key = 'SXeFdzokFTOiwJX20vGL8SaSg'
consumer_secret = 'FuGBS9b6AkDctJpubRUug3sK9q2jKvCTRNtASsTzbUbsSpBidl'
access_token = '1174161623147339776-9LgvbvJmrko3vBI7qybMc0lk4AkrBW'
access_secret = 'hp7HEU7igGorNaZseeILRHxA7TqVYecrSzpaZL0YGkK7a'

## authenticate via web browser
token <- create_token(
    app = "AnalisisNLP",
    consumer_key = consumer_key,
    consumer_secret = consumer_secret,
    access_token = access_token,
    access_secret = access_secret)

# App ---------------------------------------------------------------------

ui <- fluidPage(
    headerPanel("Aplicación para el análisis de Twitter"),
    sidebarLayout(
# Panel 1 -----------------------------------------------------------------
    sidebarPanel(
            textInput(inputId = "word",
                      label = "Hashtag"),
            sliderInput(inputId='tw',
                    label='Número de tweets para obtener:',
                    value=100,
                    min=100,
                    max=500,
                    step = 25),
            actionButton("update", "Obtener la data"),
            hr(),
            sliderInput("topMastuit", "Top de cuentas:", 
                        min=5, max= 15, value=10, step = 1)
            ),


# Panel 2 -----------------------------------------------------------------


# Main Panel --------------------------------------------------------------


mainPanel(
    tabsetPanel( 
        tabPanel("CUENTAS", plotOutput("MasTwitearon"),
                 hr(),
                 plotOutput("MasMencion")),
        tabPanel("PALABRAS", wordcloud2Output("wordPlot"))
    )
)
   


# Cierre UI ---------------------------------------------------------------
) # slider layout
) # fluidpage
