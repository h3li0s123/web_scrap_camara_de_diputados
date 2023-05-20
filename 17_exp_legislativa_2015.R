library(rvest)
library(rlist)
library(readxl)
library(tidyverse)


### Obtengo la informacion de las tablas de la pagina de la Legislatura ----


lista_lxii <- lapply(paste0("http://sitllxii.diputados.gob.mx/curricula.php?dipt=", 1:500),
       function(url){
         url %>% read_html() %>% 
           html_nodes(".textoNegro") %>%
           html_text2()
       })


###Prueba para saber si funciona ----
prueba_lxii <- lista_lxii[[1]] %>% 
  as_tibble() %>% 
  mutate(dip_fed = str_detect(value,"Diputado Federal Propietario"),
         dip_local = str_detect(value, "Diputado Local Propietario"),
         senador = str_detect(value, "Senador de la República Propietario")) %>% 
  filter(dip_fed == T | dip_local == T | senador == T)


dim(prueba_lxii)[1] == 0

###Funcion para saber si ha sido diputado o senador ----

##Crear un vector solo para guardar los datos

exp_leg_2012_prueba <- c()


##Funcion

for (i in 1:length(lista_lxii)) {
  
  a <- lista_lxii[[i]] %>% 
    as_tibble()%>% 
    mutate(dip_fed = str_detect(value,"Diputado Federal Propietario"),
           dip_local = str_detect(value, "Diputado Local Propietario"),
           senador = str_detect(value, "Senador de la República Propietario")) %>% 
    filter(dip_fed == T | dip_local == T | senador == T)
  
  #Prueba para saber si el tibble resultante está vacío o no
  
 b <-  if (dim(a)[1] == 0) {
    
    0
    
  } else {
    
    1
    
  } 
  
 ##Guardarlo en el vector vacío
 
 exp_leg_2012_prueba[i] <- b
 
}

###Guardarlo en tibble


exp_leg_2012 <- exp_leg_2012_prueba %>% 
  as_tibble()

###Expoertarlo a excell

write_excel_csv(exp_leg_2012, "experiencia_legislativa_2012.csv")

