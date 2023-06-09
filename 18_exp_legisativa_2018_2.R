library(rvest)
library(rlist)
library(readxl)
library(tidyverse)


### Obtengo la informacion de las tablas de la pagina de la Legislatura ----


lista_lxiv <- lapply(paste0("http://sitl.diputados.gob.mx/LXIV_leg/curricula.php?dipt=", 1:500),
                     function(url){
                       url %>% read_html() %>% 
                         html_nodes(".textoNegro") %>%
                         html_text()
                     })


###Prueba
prueba_lxiv <- lista_lxiv[[1]] %>% 
  as_tibble() %>% 
  mutate(dip = str_detect(value,"Diputada\\(o\\)"),
         senador = str_detect(value, "Senadora\\(o\\)")) %>% 
  filter(dip == T | senador == T)


dim(prueba_lxii)[1] == 0

###Funcion para saber si ha sido diputado o senador ----

##Crear un vector solo para guardar los datos

exp_leg_2018_prueba <- c()


##Funcion

for (i in 1:length(lista_lxiv)) {
  
  a <- lista_lxiv[[i]] %>% 
    as_tibble()%>% 
    mutate(dip = str_detect(value,"Diputada\\(o\\)"),
           senador = str_detect(value, "Senadora\\(o\\)")) %>% 
    filter(dip == T | senador == T)
  
  #Prueba para saber si el tibble resultante está vacío o no
  
  b <-  if (dim(a)[1] == 0) {
    
    0
    
  } else {
    
    1
    
  } 
  
  ##Guardarlo en el vector vacío
  
  exp_leg_2018_prueba[i] <- b
  
}

###Guardarlo en tibble


exp_leg_2018 <- exp_leg_2018_prueba %>% 
  as_tibble()

exp_leg_2018 %>% 
  print(n = Inf)


nombre_diputados_2018 <- nombre_diputados_2018 %>% 
  mutate(exp_leg= exp_leg_2018$value)

experiencia_legislativa_2018_b <- left_join(bd_joy, nombre_diputados_2018)




###Expoertarlo a excell

write_excel_csv(experiencia_legislativa_2018_b, "experiencia_legislativa_2018_b.csv")
