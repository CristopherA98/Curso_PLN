# OJO CON EL ERROR DE TOKEN INVALID
# file.remove("applications/Shiny/Twitter_NLP/.httr-oauth")
# GRACIAS STACKOVERFLOW
# https://stackoverflow.com/questions/50729821/unable-to-create-token-with-rtweet-package
# Eliminar .Renviron
# file.remove("C:/Users/Aguirre/Documents/.Renviron")
# Librerias ---------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(rtweet)
library(tidyverse)
library(wordcloud2)
library(lubridate)
library(httpuv) # para corregir el error que tenía
library(plotly)
library(tm)
library(stopwords)
library(tidytext)
library(udpipe)
library(parallel)
library(syuzhet)
# library(graph)
# library(Rgraphviz)


# AAAAAAAAAAAAAAAAAAAAAEn2KAEAAAAAVDztE4UdLxlNFpnVL%2BbX%2FmauvDg%3DmOkEtK2BdvswEUL2zNcevbPS3uSH3RwtULthxuPqWUbicjYGrl
# Credenciales ------------------------------------------------------------
# Cargado del token

consumer_key = 'XXXXXXXXXXXXXXXXXXXXX'
consumer_secret = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
access_token = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXX'
access_secret = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'

## Credenciales y creacion token
token <- create_token(
    app = "Nombre de la app",
    consumer_key = consumer_key,
    consumer_secret = consumer_secret,
    access_token = access_token,
    access_secret = access_secret)

tendencias <- get_trends(woeid = 23424801)
trend <- tendencias$trend
trend_hg <- trend[str_detect(trend, "#")]
# App ---------------------------------------------------------------------

ui <- dashboardPage(
    dashboardHeader(title = "Análisis de Twitter"),
# Menú
    dashboardSidebar(
        sidebarMenu(
            menuItem("Hashtag", tabName = "cuenta", icon = icon("twitter")),
            menuItem("Palabras", tabName = "palabra", icon = icon("th")),
            menuItem("Sentimientos",tabName = "sentimiento",icon = icon("heart")),
            menuItem("Cuentas",tabName = "cuentas",icon = icon("user-circle")),
            menuItem("Source code", icon = icon("file-code-o"), 
                     href = "https://github.com/CristopherA98/Curso_PLN/tree/main/applications/Shiny/Dashboard"),
            menuItem("Acerca de",tabName = "sobremi",icon = icon("info"))
        )
    ),
## Contenido del Menú
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "cuenta",
                    fluidRow(
                        column(width = 12,
                    # Info box 1
                        infoBoxOutput("hashtagbox"),
                    # Info box 2
                        infoBoxOutput("tweetsbox"),
                        ),
                    # Boxes
                    column(width = 12,
                        box(width = 4,
                        "Acontinuación, se presenta un menú despegable
                        en donde se encuentran los hashtag más populares
                        en el Ecuador, además se puede elegir el número de 
                        tweets que desee descargar desde la API",hr(),
                        status = "warning",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                            title = "Controles",
                        selectInput(inputId = "word",
                                      label = "Tendencias",
                                      choices = trend_hg),
                        sliderInput("slider1", "Número tweets a obtener:", 
                                        min=50, max= 1000, value=100, step = 50),
                        actionButton("update", "Obtener la data"),
                        hr(),
                        sliderInput("slider2", "Top de cuentas:", 
                                    min=5, max= 15, value=10, step = 1),
                        ),
                        box(width = 8,height = 900,
                            title = "Top de Cuentas", 
                            status = "primary", 
                            solidHeader = TRUE,
                            plotlyOutput("MasTwitearon"),
                            plotlyOutput("MasMencion")
                        ),
                        
                    )
                )
               
            ),

            # Contenido 3 PALABRAS
            tabItem(tabName = "palabra",
                    fluidRow(
                        column(width = 12,
                        box(width = 12,
                            title = "Nube de Palabras",
                            status = "danger",
                            solidHeader = TRUE,
                            wordcloud2Output("Worcloud")
                            )
                       
                        # box(width = 12,
                        #     title = "Correlaciones",
                        #     status = "success",
                        #     solidHeader = TRUE,
                        #     plotOutput("correlation")
                        #     )
                        
                        )
                 )
            ),
            
            tabItem(tabName = "sentimiento",
                    fluidPage(
                        box(width = 12,height = 500,
                        title = "Análisis de sentimientos",
                        status = "success",
                        solidHeader = TRUE,
                        plotlyOutput("sentiment")   
                        )
                    )
                  
            ),
            # Contenido 4 Cuentas
            # tabItem(tabName = "cuentas",
            #         fluidRow(
            #             box(width = 4,
            #                 "Acontinuación, puede ingresar la cuenta de su interés,
            #                 para el posterior análisis de la misma, 
            #                 además se puede elegir el número de tweets que 
            #                 desee descargar desde la API",br(),
            #                 "Nota: Ingrese el usuario sin el signo @",
            #                 hr(),
            #                 title = "Controles",
            #                 status = "warning",
            #                 solidHeader = TRUE,
            #                 hr(),
            #                 textInput("usuario","Usuario: "),
            #                 sliderInput("slider5", "Tweets a descargar:",
            #                             min=100, max= 1000, value=400, step = 50),
            #                 actionButton("update2","Obtener la data")
            #                 ),
            #             box(
            #                 title = "Frecuencia Tweets",
            #                 status = "warning",
            #                 solidHeader = TRUE,
            #                 plotOutput("frec_tw")
            #             )
            #             )
            #         ),
            # Acerca de mi
            tabItem(tabName = "sobremi",
                    fluidPage(
                        box(
                            title = "Sobre mí",
                            status = "success",
                            solidHeader = TRUE,
                            p(
                                class = "text-center",
                                img(class = "img-responsive img-rounded center-block", 
                                    src = "CristopherA98.jpeg", 
                                    style = "max-width: 150px;")
                            ),
                            p(
                                class = "text-center",
                                strong("¡Hola! Soy Cristopher Aguirre."),
                                HTML(paste0("(", 
                                            a(href = "https://twitter.com/CristopherA98", 
                                              "@CristopherA98"), ")"))
                            ),
                            
                            p(
                                "Soy un apasionado de la estadística,
                                actualmente estoy elaborando mi proyecto
                                de investigación, para obtener mi titulo
                                de Ingeniero Estadístico en la Universidad
                                Central del Ecuador"),
                            p(
                                "Durante los últimos dos años he incursionado 
                                en el mundo de los lenguajes de programación 
                                como usuario de R. Me parece un mundo muy interesante
                                y entretenido que proporciona mucho valor agregado
                                a cualquier área del conocimiento. Una de las cosas que me
                                encanta de la programación es poder compartir
                                experiencias con personas de varios lugares
                                del mundo. Pueden ponerse en contacto conmigo a través
                                de Twitter en",
                                HTML(paste0("(",
                                            a(href = "https://twitter.com/CristopherA98", 
                                              "@CristopherA98", target = "_blank"), "),")),
                                "o por correo electrónico", 
                                HTML(paste0(
                                    a(href = "mailto:cristopher_aguirre98@outlook.com", 
                                      "cristopher_aguirre98@outlook"), "."))
                                
                            )
                        ),
                        box(
                            title = "Acerca del Dashboard",
                            status = "info",
                            solidHeader = TRUE,
                            p(
                            class = "text-center",
                            img(class = "img-responsive img-rounded center-block", 
                                src = "rstudio.png", 
                                style = "max-width: 150px;")),
                            p(
                                class = "text-center",
                                img(class = "img-responsive img-rounded center-block", 
                                    src = "rtweet.png", 
                                    style = "max-width: 150px;")),
                            p(
                                "Este dashboard se lo realizó en",
                                a(href = "https://r-project.org", target = "_blank", "R"),
                                "y", 
                                a(href = "https://rstudio.com", target = "_blank", "RStudio"),
                                "usando",
                                strong("shiny,"),
                                strong("shinydashboard,"),
                                strong("rtweet,"),
                                strong("tidyverse,"),"y muchos paquetes más"
                            )
                            
                        )
                    ))
        )
    )
)