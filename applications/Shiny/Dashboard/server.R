download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv",
              "lexico_afinn.en.es.csv")

afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
    tbl_df()

# Server ------------------------------------------------------------------

server <- function(input, output) { 
    
# Info box 1    
output$hashtagbox <- renderValueBox({
        valueBox("HASHTAG",
                 input$word,
                 icon = icon("comment-dots"),
                 color = "orange")
    })
# Info box 2
output$tweetsbox <- renderValueBox({
        valueBox(input$slider1,
                 "Número de tweets", 
                 icon = icon("twitter"))
    
    })

# adicionamos el # para que se pueda buscar 
hashtag <-reactive({
    req(input$word)
})

# obtener la data con update del UI

tweets <- eventReactive(input$update,{
    
    tweets <- search_tweets2(hashtag(), 
                             input$slider1 , 
                             include_rts = FALSE,
                             langs = "es")
    tweets %>% 
        mutate(created_at = with_tz(created_at, tz = "America/Bogota"))
    
})

#---------------Cuentas que más twittearon

output$MasTwitearon <- renderPlotly({
    tweets<- data.frame(tweets())
    mastuit <- tweets %>% 
        # Agrupamiento para obtener las cuentas que más twitean
        group_by(screen_name) %>% 
        summarise(total=n()) %>% 
        arrange(desc(total)) 

    plot_ly(mastuit  [1:input$slider2,],y=~reorder(screen_name,total),x=~total,
            type = "bar",
            orientation = 'h',
            color = ~factor(total),
            colors = c("#10135B", "#15B09F")) %>% 
        layout(title = paste0("Top de Cuentas hablando de",input$word),
               xaxis = list(title = ""),
               yaxis = list(title =""),
               showlegend = FALSE)
})# fin del gráfico de mas twittearon
#---------------Obtener los que más fueron mencionados

output$MasMencion <- renderPlotly({
    tweets<- data.frame(tweets())
    masmencion <- tweets %>%
        mutate(menciones=purrr::map(.x=text,
                                    pattern='@\\w+',
                                    .f=str_extract_all)) %>% 
        dplyr::select(screen_name,menciones,created_at) %>% 
        mutate(nombre_usuario=str_to_lower(paste0('@',screen_name))) %>% 
        unnest(menciones) %>% 
        unnest(menciones) %>% 
        mutate(menciones=str_to_lower(menciones))
    
    # Gráfico cuentas más mencionadas
    menciones <-  masmencion %>% 
        group_by(menciones) %>% 
        summarise(total = n()) %>% 
        arrange(desc(total))
    
    plot_ly(menciones [1:input$slider2,],y=~reorder(menciones,total),x=~total,
            type = "bar",
            orientation = 'h',
            color = ~factor(total),
            colors = c("#10135B", "#15B09F")) %>% 
        layout(title = paste0("Top de Cuentas más mencionadas con el ",input$word),
               xaxis = list(title = ""),
               yaxis = list(title =""),
               showlegend = FALSE)
    
}) # fin del gráfico de mas mencion

#---------------World Cloud

output$Worcloud <- renderWordcloud2({
    tweets <- data.frame(tweets())
#------------ CORPUS
cleanStrVec <- function(string_vec) {
    clean_vec <- c()
    for (k in c(1:length(string_vec))) {
        n <-iconv(string_vec[k], "latin1", "ASCII",sub='')
        clean_vec <- c(clean_vec, n)
    }
    return(clean_vec)
} 

text <- tweets$text
cleaned_text<-cleanStrVec(text)
docs <- VCorpus(VectorSource(cleaned_text))

docs <- VCorpus(VectorSource(text))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

# remover URLS
removeURL <- function(x) gsub("http\\S*", "", x)
docs <- tm_map(docs, content_transformer(removeURL))
# remover menciones
removeMenciones <- function(x) gsub("@\\S*", "", x)
docs <- tm_map(docs, content_transformer(removeMenciones))
# transformar a minusculas
docs <- tm_map(docs, content_transformer(tolower))
# remueve numeros
docs <- tm_map(docs, removeNumbers)
# remueve stopwords
docs <- tm_map(docs, removeWords, stopwords("es"))
# remueve puntuaciones
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
term.freq <- rowSums(as.matrix(dtm))
term.freq <- subset(term.freq, term.freq >=5)
data <- data.frame(term = names(term.freq), freq = term.freq)

data %>% wordcloud2(size = 2,color = "random-dark",
                     shape = "circle",
            minRotation = -pi/6,
            maxRotation = -pi/6,
            rotateRatio = 1)
})

#---------------Sentimientos
output$sentiment <- renderPlotly({
    tweets<- data.frame(tweets())
    
    tweets_sentimiento <- tweets %>%
        select(screen_name,status_id,text) %>%
        rowid_to_column() %>%
        tidytext::unnest_tokens(Word, text)
    
    #### Cambio de nombre para poder hacer join con las palabras de los tweets
    #names (afinn)[1] = "words"
    
    tuits_afinn = tweets_sentimiento %>%
        inner_join(afinn, ., by = "Word") %>%
        mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa"))
    
    
    tema_graf <-
        theme_minimal() +
        theme(text = element_text(family = "serif"),
              panel.grid.minor = element_blank(),
              strip.background = element_rect(fill = "#EBEBEB", colour = NA),
              legend.position = "none",
              legend.box.background = element_rect(fill = "#EBEBEB", colour = NA))
    
    
    tuits_afinn <-  tuits_afinn %>%
        group_by(Tipo) %>%
        summarise(n=n(),
                  porc= (n/nrow(tuits_afinn))*100) 
        
    plot_ly(tuits_afinn,y=~Tipo,x=~porc,
            type = "bar",
            orientation = 'h',
            color = ~factor(Tipo),
    colors = c("#10135B", "#15B09F")) %>% 
        layout(title = paste0("Sentimiento de los Tweets del", input$word),
               xaxis = list(title = "% de conversaciones"),
               yaxis = list(title =""),
               showlegend = FALSE)

})



# Cierre App --------------------------------------------------------------

    
    }