library(rvest)
library(rlist)
library(tidyverse)
library(lubridate)

#### Nombre de diputados 2015 ----

nom_dip_2015 <- lapply(paste0("http://sitllxiii.diputados.gob.mx/curricula.php?dipt=", 1:500),
                   function(url){
                     url %>% read_html() %>% 
                       html_nodes("font strong") %>% 
                       html_text() %>%
                       gsub('[\r\n\t]', '', .)
                     
                   })


nom_dips_2015 <- nom_dip_2015 %>% 
  unlist() %>% 
  as_tibble() %>% 
  mutate(value = str_remove_all(value, "\\(LICENCIA\\)"),
         value = str_remove_all(value, "Dip. "))


write_excel_csv(nom_dips_2015,"nombre_diputados_2015.csv")



### Entidad diputados 2015 ----


entidad_dips_2015 <- lapply(paste0("http://sitllxiii.diputados.gob.mx/curricula.php?dipt=", 1:500),
       function(url){
         url %>% read_html() %>% 
           html_nodes("td tr:nth-child(3) strong:nth-child(1) font") %>% 
           html_text() %>%
           gsub('[\r\n\t]', '', .)
         
       })

entidad_dips_2015 <- entidad_dips_2015 %>% 
  unlist() %>% 
  as_tibble()

write_excel_csv(entidad_dips_2015,"entidad_diputados_2015.csv")


#### Distrito dips 2015 ----


dist_circ_2015 <- lapply(paste0("http://sitllxiii.diputados.gob.mx/curricula.php?dipt=", 1:500),
       function(url){
         url %>% read_html() %>% 
           html_nodes("strong:nth-child(4) font") %>% 
           html_text() %>%
           gsub('[\r\n\t]', '', .)
         
       })

dist_circ_2015 <- dist_circ_2015 %>% 
  unlist() %>% 
  as_tibble()


write_excel_csv(dist_circ_2015,"distr_circ_2015.csv")



#### Edad dips 2015 ----

edad_dip_2015 <- lapply(paste0("http://sitllxiii.diputados.gob.mx/curricula.php?dipt=", 1:500),
       function(url){
         url %>% read_html() %>% 
           html_nodes("tr:nth-child(5) strong font") %>% 
           html_text() %>%
           gsub('[\r\n\t]', '', .)
         
       })



edad_dip_2015 <- edad_dip_2015 %>% 
  unlist() %>% 
  as_tibble()


edad_dip_2015 <- edad_dip_2015 %>% 
  rename(fecha_nacimiento = "value") %>% 
  mutate(fecha_nacimiento = str_remove_all(fecha_nacimiento, " ")) %>% 
  mutate(fecha_nacimiento_1 = dmy(fecha_nacimiento),
         año = year(fecha_nacimiento_1),
         años_dip = 2018-año) %>% 
  select(-año, -fecha_nacimiento)


write_excel_csv(edad_dip_2015,"edad_dip_2015.csv")


### Nivel de educacion dips 2015 ----

edu_dips_2015 <- lapply(paste0("http://sitllxiii.diputados.gob.mx/curricula.php?dipt=", 1:500),
       function(url){
         url %>% read_html() %>% 
           html_nodes("tr:nth-child(4) .textoNegro:nth-child(1)") %>% 
           html_text() %>%
           gsub('[\r\n\t]', '', .)
         
       })

edu_dips_2015 %>% 
  unlist() %>% 
  as_tibble()


edu_dip_2015 <- edu_dip_2015 %>% 
  unlist() %>% 
  as_tibble()



write_excel_csv(edu_dip_2015,"edu_dip_2015.csv")


### Optener partido de dips 2015 ----

##Diputados del PRI

url <- "http://sitllxiii.diputados.gob.mx/listado_diputados_gpnp.php?tipot=1"


pri_dips_2015 <- url %>% read_html() %>% 
  html_nodes(".linkVerde") %>% 
  html_text() %>% 
  unlist() %>% 
  as_tibble()


pri_dips_2015 <- pri_dips_2015 %>% 
  rename(nom_dip = "value") %>% 
  mutate(nom_dip = str_remove_all(nom_dip, "\\d")) %>% 
  separate(col = "nom_dip",
           into = c("blanco","apellido_1", "apellido_2", "nombre_1", "nombre_2", "nombre_3", "nombre_4"),
           sep = " ") %>%
  select(nombre_1, nombre_2, nombre_3, nombre_4, apellido_1, apellido_2) %>% 
  unite(col = "diputado_pri",
        sep = " ") %>%
  mutate(diputado_pri = str_replace_all(diputado_pri, "NA", ""),
         diputado_pri = str_remove_all(diputado_pri, "\\(LICENCIA\\)"))


write_excel_csv(pri_dips_2015,"pri_dips.csv")

##Diputados del PAN

url <- "http://sitllxiii.diputados.gob.mx/listado_diputados_gpnp.php?tipot=3"


pan_dips_2015 <- url %>% read_html() %>% 
  html_nodes(".linkVerde") %>% 
  html_text() %>% 
  unlist() %>% 
  as_tibble()



pan_dips_2015 <- pan_dips_2015 %>% 
  rename(nom_dip = "value") %>% 
  mutate(nom_dip = str_remove_all(nom_dip, "\\d")) %>% 
  separate(col = "nom_dip",
           into = c("blanco","apellido_1", "apellido_2", "nombre_1", "nombre_2", "nombre_3", "nombre_4"),
           sep = " ") %>%
  select(nombre_1, nombre_2, nombre_3, nombre_4, apellido_1, apellido_2) %>% 
  unite(col = "diputado_pri",
        sep = " ") %>%
  mutate(diputado_pri = str_replace_all(diputado_pri, "NA", ""),
         diputado_pri = str_remove_all(diputado_pri, "\\(LICENCIA\\)"),
         partido = rep("PAN", length(pan_dips_2015)))



### Dips PRD


url <- "http://sitllxiii.diputados.gob.mx/listado_diputados_gpnp.php?tipot="


pan_dips_2015 <- url %>% read_html() %>% 
  html_nodes(".linkVerde") %>% 
  html_text() %>% 
  unlist() %>% 
  as_tibble()



pan_dips_2015 <- pan_dips_2015 %>% 
  rename(nom_dip = "value") %>% 
  mutate(nom_dip = str_remove_all(nom_dip, "\\d")) %>% 
  separate(col = "nom_dip",
           into = c("blanco","apellido_1", "apellido_2", "nombre_1", "nombre_2", "nombre_3", "nombre_4"),
           sep = " ") %>%
  select(nombre_1, nombre_2, nombre_3, nombre_4, apellido_1, apellido_2) %>% 
  unite(col = "diputado_pri",
        sep = " ") %>%
  mutate(diputado_pri = str_replace_all(diputado_pri, "NA", ""),
         diputado_pri = str_remove_all(diputado_pri, "\\(LICENCIA\\)"),
         partido = rep("PAN", length(pan_dips_2015)))



tot_partidos_dips_2015 <- lapply(paste0("http://sitllxiii.diputados.gob.mx/listado_diputados_gpnp.php?tipot=", c(1,3,2,14,5,6,12,15,9,16)),
       function(url){
         url %>% read_html() %>% 
           html_nodes(".linkVerde") %>% 
           html_text() %>%
           gsub('[\r\n\t]', '', .)
         
       })


tot_partidos_dips_2015 <- tot_partidos_dips_2015 %>% 
  unlist() %>% 
  as_tibble() %>%
  rename(nom_dip = "value") %>% 
  mutate(nom_dip = str_remove_all(nom_dip, "\\d")) %>% 
  separate(col = "nom_dip",
           into = c("blanco","apellido_1", "apellido_2", "nombre_1", "nombre_2", "nombre_3", "nombre_4", "nombre_5"),
           sep = " ") %>%
  select(nombre_1, nombre_2, nombre_3, nombre_4, nombre_5,apellido_1, apellido_2) %>% 
  unite(col = "nom_dip",
        sep = " ") %>%
  mutate(nom_dip = str_replace_all(nom_dip, "NA", ""),
         nom_dip = str_remove_all(nom_dip, "\\(LICENCIA\\)"),
         partido = c(rep("PRI", 202),
                     rep("PAN", 107),
                     rep("PRD", 51),
                     rep("MORENA", 50),
                     rep("PVEM", 38),
                     rep("MC", 21),
                     rep("Nueva Alianza", 13),
                     rep("PES", 11),
                     rep("IND", 1),
                     rep("SP", 6))) %>% 
  print(n = Inf)


write_excel_csv(tot_partidos_dips_2015, "partidos_dips_2015.csv")

