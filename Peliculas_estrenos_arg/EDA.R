library("lubridate")

format(Fecha_Estreno, "%Y-%m") # convierte a 2000-January

tibble_all_final %>% 
  group_by(Nacionalidad) %>% 
  count(.) %>% 
  arrange(desc(n))


aa <- tibble_all_final %>%
  group_by( Fecha_Estreno = year(Fecha_Estreno) , Nacionalidad) %>%
  filter(Fecha_Estreno > "2015-01-01")  %>% 
  count(.) %>% 
  arrange(Fecha_Estreno, desc(n))

aa <- tibble_all_final %>%
  group_by( Fecha_Estreno  , Nacionalidad) %>%
  filter(Fecha_Estreno >= "2018-01-01")  %>% 
  count(.) %>%
  filter(Nacionalidad %in% c("estados-unidos","argentina"))


tibble_all_final %>%
group_by( Fecha_Estreno = year(Fecha_Estreno) , Nacionalidad) %>%
filter(Fecha_Estreno > "2015-01-01")  %>% 
count(.) %>%
filter(Nacionalidad %in% c("estados-unidos","argentina","francia","espana","reino-unido","italia","alemania") ) %>%
arrange(Fecha_Estreno) %>%
ggplot(.) +
geom_line(aes(x=Fecha_Estreno, y = n ,colour=Nacionalidad),stat = "identity")



