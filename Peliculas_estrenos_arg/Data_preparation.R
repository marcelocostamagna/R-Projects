library("lubridate")
library("readxl")
library("lubridate")
library("pdftools")

# FUENTE 1 : read and tidy scrapped data from cinesargentinos.com.ar
tibble_all <- readRDS("tibble_all.rds")

tibble_all_interm <- tibble_all %>% 
  mutate(a = paste(Año,Mes_num, sep = "-"), b = parse_date_time(a,"ym"), Fecha_Estreno = ymd(b)) %>% #formater fechas de estreno
  mutate(Nro_Pelicula = str_remove(str_sub( link, start = 45, end = 48),"-")) %>% #formatear número de película
  mutate(a = str_remove(str_sub( link, start = 49),"/"), b = str_replace_all(a, "-"," "), c = str_trim(b), Pelicula = str_to_upper(c) ) %>% #formatear nombre de película
  mutate(Coproduccion = ifelse(is.na(Nacionalidad), "SI","NO" )) %>% #nueva columna indicando si es o no coproducción de varios países
  mutate(a = paste(Nacionalidad1, Nacionalidad2, Nacionalidad3, Nacionalidad4, Nacionalidad5, Nacionalidad6, sep = " "), b = str_replace_all(a,"NA", " "),  Coproduccion_Paises = str_trim(b)  ) %>% #concatena todos los países particioantes de la coproducción
  mutate( a = ifelse(str_detect(Coproduccion_Paises, "estados-unidos")==T,"SI", "NO"  ), b = ifelse(a =="SI", "estados-unidos", Nacionalidad ), Nac_2 = ifelse(is.na(b), word(Coproduccion_Paises, 1), b )  ) %>%  # coproducciones de varios países, si existe EEUU loa signa sino asigna el primer país que aparece en el listado 
  select( Fecha_Estreno, Nro_Pelicula, Pelicula,Coproduccion,  Nacionalidad = Nac_2 , Coproduccion_Paises) 


duplicados <- duplicated(tibble_all_interm[,2])
tibble_all_final <- tibble_all_interm[!duplicados,]


saveRDS(tibble_all_final, "tibble_all_final.rds")

# FUENTE 2 - Opcion 1 : Cargar datos de PDF obtenido de Tableau en la página oficial INCAA---------------------------------------------------
##http://fiscalizacion.incaa.gov.ar/index_estadisticas_peliculas.php
##filtrado 2007-2018
##https://medium.com/@CharlesBordet/how-to-extract-and-clean-data-from-pdf-files-in-r-da11964e252e
##https://www.brodrigues.co/blog/2019-03-31-tesseract/
raw_text <- pdf_text("Datos_Incaa/Ranking_Peliculas.pdf")
raw_text_2 <- strsplit(raw_text, "\n")
raw_text_3 <- unlist(raw_text_2)
raw_text_4 <- str_replace_all(raw_text_3, "\t", " ")
raw_text_5 <- str_replace_all(raw_text_4, "\"", " ")
raw_text_5 <- tibble(raw_text_5)

# crear data frame
raw_incaa_tibble <- raw_text_5 %>%
  transmute(tmp1 = str_trim(.$raw_text_5)) %>% 
  mutate( 
    "Ranking" = str_sub(tmp1 ,1 ,5 ),
    "Titulo" = str_sub(tmp1 ,6 ,44 ),
    "Fecha_Estreno" = str_sub(tmp1 ,-73 ,-56 ),
    "Origen" = str_sub(tmp1 ,-55 ,-51 ),
    "Entradas_Total" = str_sub(tmp1 ,-34 ,-12 ),
    "Recaudacion" = str_sub(tmp1 ,-12 ,-1 )
  ) %>% 
  select(-tmp1)

#convert whitespace to NA,  remove NA, clean date, remove dots and format
incaa_tibble <-  raw_incaa_tibble %>% 
  mutate_all(str_trim) %>% 
  mutate_all(na_if,"") %>% 
  drop_na(.) %>% 
  filter(Origen %in% c("Ext","Nac")) %>% 
  mutate( Ranking = str_remove_all(.$Ranking, "[.]"),
          Titulo =str_replace_all(.$Titulo, "[]", "\'"),
          Fecha_Estreno = str_replace_all(Fecha_Estreno, "[^0-9/]", ""),
          Entradas_Total = str_remove_all(.$Entradas_Total, "[.]"),
          Recaudacion = str_remove_all(.$Recaudacion, "[.]")
        ) %>% 
  mutate( Ranking = as.numeric(Ranking),
          Titulo = Titulo,
          Fecha_Estreno =as.Date.character(Fecha_Estreno,  "%d/%m/%Y"),
          Origen = as.factor(Origen),
          Entradas_Total = as.numeric(Entradas_Total ),
          Recaudacion = as.numeric(Recaudacion)
        )
  #drop_na(.) # elimina títulos "Ciclos.."
       
 
# FUENTE 2 - Opcion 2 : Cargar datos de excel obtenido desde la página de INCAA---------------------------------------------------
#http://fiscalizacion.incaa.gov.ar/index_estadisticas_peliculas.php
#filtrado 2007-2018

Ranking_Peliculas <- read_excel("Datos_Incaa/Ranking_Peliculas.xlsx", 
                                col_types = c("numeric", "text", "date", 
                                              "text", "numeric", "numeric"))

# Tables Joins --------------------------------------------------------------

#antes de unir hay que sumar el total de espectadores pde la misma películam ej: gravedad 
#hay que remover "3D" en varios títulos
#quitar acentos y eñes y dos puntos

cc <- tibble_all_final %>% 
  filter(Fecha_Estreno > "2007-01-01") %>% 
  full_join(Ranking_Peliculas, by = c("Pelicula" = "Titulo"))
  
  



