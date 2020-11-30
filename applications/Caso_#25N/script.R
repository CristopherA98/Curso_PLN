#Librerias que se van a usar para todo el análisis
pckg <- c("easypackages","tidyverse","rvest","purrr")
# install.packages("pckg") desomentar en caso de no tener innstalados los paquetes
library(easypackages)
libraries(pckg)

# Definición de funciones
obtieneNoticiasBusqueda <-  function(busqueda){
  news_pag = "https://news.google.com/"
  parametro_busqueda = "search?q="
  busqueda_no_espacios = gsub(" ","%20", busqueda)
  parametro_final = "&hl=es-419&gl=US&ceid=US:es-419"
  html_dir = paste0(news_pag,parametro_busqueda,busqueda_no_espacios,parametro_final)
  google_news = read_html(html_dir)
  noticias = google_news %>% 
    html_nodes(css = ".xP6mwf") %>% 
    html_children()
  noticiasDF = map(noticias,obtieneNoticiasData)
  noticiasDF = bind_rows(noticiasDF)
  noticiasDF = noticiasDF[!is.na(noticiasDF$Titular),]
  return(noticiasDF)
}


obtieneNoticiasData = function(noticia){
  news_pag = "https://news.google.com/"
  titular = noticia %>% html_node("h3") %>% html_text()
  fecha = noticia %>% html_node("time") %>% html_attr("datetime")
  diario = noticia %>% html_node("a.wEwyrc.AVN2gc.uQIVzc.Sksgp") %>% html_text()
  link_enmascarado = noticia %>% html_node("h3 a") %>% html_attr("href")
  link_enmascarado = paste0(news_pag,substring(link_enmascarado,3))  
  link_leido = read_html(link_enmascarado)
  link = link_leido %>% 
    html_nodes(css='a') %>% 
    tail(1) %>% 
    html_attr("href")
  noticiaDF = data.frame(Titular=titular, Fecha=fecha, Diario=diario, Link=link, stringsAsFactors = F)
  return(noticiaDF)
}

noticiasMujer <- obtieneNoticiasBusqueda("Violencia contra la mujer Ecuador")

# Diario con mas noticias publicadas del tema
noticiasMujer %>% 
  count(Diario) %>% 
  slice_max(order_by = n,n=5)

# Diarios
Diarios <-  c("El Comercio (Ecuador)", "El Telégrafo (por eliminar)", 
            "El Universo","La Hora (Ecuador)","Primicias")
Estructura = data.frame(Diario=Diarios)
Estructura$CSS = NA
Estructura$CSS[Estructura$Diario=='El Comercio (Ecuador)'] = '.paragraphs'
Estructura$CSS[Estructura$Diario=='El Telégrafo (por eliminar)'] = '.itemFullText'
Estructura$CSS[Estructura$Diario=='El Universo'] = '.field-name-body'
Estructura$CSS[Estructura$Diario=='La Hora (Ecuador)'] = '#contenedorGeneral'
Estructura$CSS[Estructura$Diario=='Primicias'] = '#entry-content-inarticle'


obtenerNoticiaNacional = function(link_noticia, diario, diccionario_css){
  
  noticia_leida = read_html(link_noticia)
  css = diccionario_css$CSS[diccionario_css$Diario==diario]
  
  text_nodes = noticia_leida %>% 
    html_nodes(css = css) %>% 
    html_nodes("p")
  
  text = text_nodes %>% 
    html_text()
  
  text = paste0(text, collapse = " ")
  
  return(text)
  
}

noticiasMujer <-  noticiasMujer %>% filter(Diario %in% Diarios)
news = map2_chr(noticiasMujer$Link, noticiasMujer$Diario, obtenerNoticiaNacional, diccionario_css=Estructura)
noticiasMujer$Noticia = news

sapply(noticiasMujer, function(x) sum(is.na(x)))
which(is.na(noticiasMujer$Fecha))
noticiasMujer[54,4]
# puede que sea por la fecha de publicacion revisar mañana

# fecha de corte 28 de noviembre / fecha inicio 5 marzo 
saveRDS(noticiasMujer, "applications/Caso_#25N/noticiasMujer.RDS")

# LEER DATOS
rm(list=ls())
noticiasMujer = readRDS("applications/Caso_#25N/noticiasMujer.RDS")

