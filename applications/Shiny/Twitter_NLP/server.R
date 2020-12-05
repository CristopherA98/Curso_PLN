
# Funciones de limpieza ---------------------------------------------------

#### Limpieza de texto y tokenización ####
limpiar_tokenizar <- function(texto){
    # El orden de la limpieza no es arbitrario
    # Se convierte todo el texto a minúsculas
    nuevo_texto <- tolower(texto)
    # Eliminación de páginas web (palabras que empiezan por "http." seguidas
    # de cualquier cosa que no sea un espacio)
    nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
    # Eliminación de signos de puntuación
    nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
    # Eliminación de números
    nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
    # Eliminación de espacios en blanco múltiples
    nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
    # Tokenización por palabras individuales
    nuevo_texto <- str_split(nuevo_texto, " ")[[1]]
    # Eliminación de tokens con una longitud < 2
    nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 1})
    return(nuevo_texto)
}


# Server  -----------------------------------------------------------------

server <- function(input, output) {
    
# adicionamos el # para que se pueda buscar 
    hashtag <-reactive({
        req(input$word)
        paste("#",input$word)
    })
    
# obtener la data con update del UI

tweets <- eventReactive(input$update,{
        
        tweets <- search_tweets2(hashtag(), 
                                 input$tw , 
                                 include_rts = FALSE,
                                 langs = "es")
        tweets %>% 
            mutate(created_at = with_tz(created_at, tz = "America/Bogota"))
    
})

# Cuentas que más twittearon
        
output$MasTwitearon <- renderPlot({
tweets<- data.frame(tweets())
mastuit <- tweets %>% 
    # Agrupamiento para obtener las cuentas que más twitean
    group_by(screen_name) %>% 
    summarise(total=n()) %>% 
    arrange(desc(total)) 

    # Gráfico más twittearon
ggplot(mastuit  [1:input$topMastuit,],
       aes(x=reorder(screen_name,total),y=total,
           label=total,
           fill = total))+
        geom_bar(stat="identity", colour = "black")+
        scale_fill_distiller(palette = "Spectral")+
        theme_classic() +
        theme(legend.position = "none")+
        geom_text(aes(label=total), hjust=1.5, colour="black", size=4)+
        coord_flip()+
        theme(plot.title = element_text(face = "bold", size = 16)) +
        theme(axis.text = element_text(size=12,colour = "black"))+

        # geom_label( colour = "black")+
        # theme(plot.title = element_text(face = "bold", size = 16)) +
        # theme(axis.text = element_text(size=12))+
        labs(x = NULL, y = NULL,
            title = "Top de cuentas",
            subtitle = paste0("Tweets hablando de #",input$word),
            caption = "\nSource: API twitter \nData obtenida via rtweet")
})# fin del gráfico de mas twittearon

    # Obtener los que más fueron mencionados

output$MasMencion <- renderPlot({
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
    
ggplot(menciones [1:input$topMastuit,],
        aes(reorder(menciones,total),total,label=total,fill = total))+
        geom_bar(stat="identity",colour = "black")+ 
        scale_fill_distiller(palette = "Spectral")+
        geom_text(aes(label=total), hjust=1.5, colour="black", size=4)+
        theme_classic() +
        theme(legend.position = "none")+
        coord_flip()+
        theme(plot.title = element_text(face = "bold", size = 16)) +
        theme(axis.text = element_text(size=12,colour = "black"))+
        labs(
            x = NULL, y = NULL,
            title = "Top cuentas más mencionadas",
            subtitle = paste0("En los tweets hablando de #",
                              input$word),
            caption ="\nSource: API twitter \nData obtenida via rtweet")
    
}) # fin del gráfico de mas mencion

# Wordcloud
output$wordPlot <- renderWordcloud2({
    #Selección de variables
    tweets<- as.data.frame(tweets())
    tweets1 <- tweets %>% select(screen_name, created_at, status_id, text)
    
    #Se renombran las variables con nombres más prácticos
    tweets1 <- tweets1 %>% rename(autor = screen_name, fecha = created_at,
                                  texto = text, tweet_id = status_id)
    
    tweets1 <- tweets1 %>% dplyr::mutate(texto_tokenizado = map(.x = texto,
                                                                .f = limpiar_tokenizar))
    
    tweets_tidy <- tweets1 %>% select(-texto) %>% unnest()
    tweets_tidy <- tweets_tidy %>% rename(token = texto_tokenizado)
    
    ### Filtramos StopWords ###
    tweets_tidy <- tweets_tidy %>% filter(!(token %in% tm::stopwords(kind="es")))
    
    ### Word Clouds ### Nube de palabras ### 
    
    df_grouped <- tweets_tidy %>% group_by(token) %>% count(token) %>%
        mutate(frecuencia = n / n()) %>%
        arrange(desc(frecuencia))
    
    df_frame <- as.data.frame(df_grouped)
    df_frame %>% select(token,n) %>%
        wordcloud2(minSize = 5,shape = "circle",size = .7)
    
    
})


# Cerrar SERVER -----------------------------------------------------------


    
}