library("rvest")
library("tidyverse")
# will be use to make HTML GET and POST requests
library("httr")
library("curl")
library("ggplot2")


# save and load rdata -----------------------------------------------------

saveRDS(tibble_all, "tibble_all.rds")
tibble_all <- readRDS("tibble_all.rds")

# iterar para generar los linksde películas por mes y año -----------------------------


url_año <- 2000:2018
url_mes <- factor(c("enero","febrero","marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"))

tibble_all_links <- tibble(Año = integer(), Mes = factor(), Mes_num = integer(), link=character())

for (i in 1:length(url_año)) {
  for (j in 1:length(url_mes)) {
    
    #Specifying the url for desired website to be scraped
    url <- paste0('https://www.cinesargentinos.com.ar/cartelera/my:', url_mes[j], '-', url_año[i],'_ne:0/')
    
    #Reading the HTML code from the website
    webpage <- read_html(curl(url))
    
    all_links <-  webpage %>% 
      html_nodes("a") %>%
      html_attr('href') 
    
    links <- str_detect(all_links, 'com.ar/pelicula/') %>% #crear vector booleano solo links a películas
      all_links[.]                               #traer solo links
    
    links <- links[!duplicated(links)]  #crea vector booleano de duplicados y filtra
    
    tibble_all_links <- bind_rows(tibble_all_links, tibble(Año=url_año[i], Mes=url_mes[j], Mes_num=j, link=links))
    
    #end for
  }
}

saveRDS(tibble_all_links, "Datos_scrapping/tibble_all_links.rds")


# iterar para obtener datos de películas ----------------------------------

#Specifying the url for desired website to be scraped
#url_pel <- paste0('https://www.cinesargentinos.com.ar/cartelera/my:', url_mes[j], '-', url_año[i],'_ne:0/')
#url_pel <- 'https://www.cinesargentinos.com.ar/pelicula/4109-cuando-te-encuentre/'


tibble_all_peli_datos <- tibble(URL = character(), Nacionalidad = character(), Genero = character())

for (i in 1:nrow(tibble_all_links)) {
  url_pel = tibble_all_links$link[i]
  #Reading the HTML code from the website
  webpage_pel <- read_html(curl(url_pel))
  
  all_links <-  webpage_pel %>% 
    html_nodes("a") %>%
    html_attr('href') 
  
  links_nac <- str_detect(all_links, '/nacionalidad/') %>% #crear vector booleano solo link nacionalidad
    all_links[.] %>%                 #traer solo links
    .[!is.na(.)]                     #elimina NAs
  nacionalidad <- links_nac %>% str_sub( ., start = 15) # obtener nacionalidad
  
  links_gen <- str_detect(all_links, '/genero/') %>% #crear vector booleano solo link genero
    all_links[.]   %>%         #traer solo links
    .[!is.na(.)]               #elimina NAs
  tmp_gen <-  links_gen %>% 
    str_sub( ., start = 9)   # obtener nacionalidad/es 
  genero <-  ifelse(length(tmp_gen) > 1, str_flatten(tmp_gen,  collapse = "-"), ifelse(length(tmp_gen)==1,tmp_gen[1],NA)  )   # concatenar géneros  
  
  tibble_all_peli_datos <- bind_rows(tibble_all_peli_datos, c(URL = url_pel, Nacionalidad = nacionalidad, Genero = genero))
  
  #end for  
}

saveRDS(tibble_all_peli_datos, "Datos_scrapping/tibble_all_peli_datos.rds")

# bind tables -------------------------------------------------------------

tibble_all <- bind_cols(tibble_all_links, tibble_all_peli_datos)

# Data Transformation -----------------------------------------------------


# test --------------------------------------------------------------------



for (i in 1:nrow(tibble_all_links_mayo_2013)) {
  print(tibble_all_links_mayo_2013$link[i])
}

#generar todos los links por mes y año
vec <- vector(mode="character", length=0)
#listado completo de links por mes y año
for (i in 1:length(url_año)) {
  for (j in 1:length(url_mes)) {
    vec <- c(vec, paste0('https://www.cinesargentinos.com.ar/cartelera/my:', url_mes[j], '-', url_año[i],'_ne:0/'))
  }
}


