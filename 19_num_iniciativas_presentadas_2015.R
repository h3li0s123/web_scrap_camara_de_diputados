library(rvest)
library(tidyverse)
library(readxl)


### Iniciativas del PRI de 2015 ----

iniciativas_pri <- lapply(paste0("http://sil.gobernacion.gob.mx/Numeralia/Iniciativas/resultadosNumeraliaIniciativas.php?SID=&Origen=IL&Serial=18be67fea0667d8bcc1808f56d39d157&Reg=1784&Paginas=15&pagina=", 1:119),
       function(url){
         url %>% read_html() %>% 
           html_nodes("td:nth-child(7)") %>% 
           html_text() %>%
           gsub('[\r\n\t]', '', .)
         
       })

### Iniciativas del PAN de 2015 ----

iniciativas_pan <- lapply(paste0("http://sil.gobernacion.gob.mx/Numeralia/Iniciativas/resultadosNumeraliaIniciativas.php?SID=&Origen=IL&Serial=23f55647b052151e24ef2fcb3b0756ac&Reg=1001&Paginas=15&pagina=", 1:67),
                          function(url){
                            url %>% read_html() %>% 
                              html_nodes("td:nth-child(7)") %>% 
                              html_text() %>%
                              gsub('[\r\n\t]', '', .)
                            
                          })

### Iniciativas del PRD de 2015 ----

iniciativas_prd <- lapply(paste0("http://sil.gobernacion.gob.mx/Numeralia/Iniciativas/resultadosNumeraliaIniciativas.php?SID=&Origen=IL&Serial=f2e46ad37667a0245f59940dea45e6dd&Reg=727&Paginas=15&pagina=", 1:49),
                          function(url){
                            url %>% read_html() %>% 
                              html_nodes("td:nth-child(7)") %>% 
                              html_text() %>%
                              gsub('[\r\n\t]', '', .)
                            
                          })

### Iniciativas del Pvem de 2015 ----

iniciativas_pvem <- lapply(paste0("http://sil.gobernacion.gob.mx/Numeralia/Iniciativas/resultadosNumeraliaIniciativas.php?SID=&Origen=IL&Serial=b66ed2ec70563946d39f9a5d2a5268d5&Reg=471&Paginas=15&pagina=", 1:32),
                          function(url){
                            url %>% read_html() %>% 
                              html_nodes("td:nth-child(7)") %>% 
                              html_text() %>%
                              gsub('[\r\n\t]', '', .)
                            
                          })


### Iniciativas del PT de 2015 ----

iniciativas_pt <- lapply(paste0("http://sil.gobernacion.gob.mx/Numeralia/Iniciativas/resultadosNumeraliaIniciativas.php?SID=&Origen=IL&Serial=8842b7be36fd84c9a0e4315a8bccbd71&Reg=11&Paginas=15&pagina=", 1),
                           function(url){
                             url %>% read_html() %>% 
                               html_nodes("td:nth-child(7)") %>% 
                               html_text() %>%
                               gsub('[\r\n\t]', '', .)
                             
                           })


### Iniciativas del PANAL de 2015 ----

iniciativas_panal <- lapply(paste0("http://sil.gobernacion.gob.mx/Numeralia/Iniciativas/resultadosNumeraliaIniciativas.php?SID=&Origen=IL&Serial=85c4dd0712ca7d56768f7d796d6874c7&Reg=267&Paginas=15&pagina=", 1:18),
                         function(url){
                           url %>% read_html() %>% 
                             html_nodes("td:nth-child(7)") %>% 
                             html_text() %>%
                             gsub('[\r\n\t]', '', .)
                           
                         })


### Iniciativas del MC de 2015 ----

iniciativas_mc <- lapply(paste0("http://sil.gobernacion.gob.mx/Numeralia/Iniciativas/resultadosNumeraliaIniciativas.php?SID=&Origen=IL&Serial=f79a8f4f259bbd78e523ef14e25bfaca&Reg=817&Paginas=15&pagina=", 1:55),
                            function(url){
                              url %>% read_html() %>% 
                                html_nodes("td:nth-child(7)") %>% 
                                html_text() %>%
                                gsub('[\r\n\t]', '', .)
                              
                            })



### Iniciativas del morena de 2015 ----

iniciativas_morena <- lapply(paste0("http://sil.gobernacion.gob.mx/Numeralia/Iniciativas/resultadosNumeraliaIniciativas.php?SID=&Origen=IL&Serial=75bb516fd10bc5ad2f947bc70ed21db4&Reg=444&Paginas=15&pagina=", 1:30),
                         function(url){
                           url %>% read_html() %>% 
                             html_nodes("td:nth-child(7)") %>% 
                             html_text() %>%
                             gsub('[\r\n\t]', '', .)
                           
                         })

### Iniciativas del pes de 2015 ----

iniciativas_pes <- lapply(paste0("http://sil.gobernacion.gob.mx/Numeralia/Iniciativas/resultadosNumeraliaIniciativas.php?SID=&Origen=IL&Serial=b6f4768ac5f7b2288bf4e8ea129ba690&Reg=227&Paginas=15&pagina=", 1:16),
                             function(url){
                               url %>% read_html() %>% 
                                 html_nodes("td:nth-child(7)") %>% 
                                 html_text() %>%
                                 gsub('[\r\n\t]', '', .)
                               
                             })

### Iniciativas del sin partido de 2015 ----

iniciativas_sp <- lapply(paste0("http://sil.gobernacion.gob.mx/Numeralia/Iniciativas/resultadosNumeraliaIniciativas.php?SID=&Origen=IL&Serial=f393f054b68a97752af6072271d1bdff&Reg=15&Paginas=15&pagina=", 1),
                          function(url){
                            url %>% read_html() %>% 
                              html_nodes("td:nth-child(7)") %>% 
                              html_text() %>%
                              gsub('[\r\n\t]', '', .)
                            
                          })



#### Convertir en tibble y hacer uno solo ----


iniciativas_pri <- iniciativas_pri %>%
  unlist() %>% 
  as_tibble()

iniciativas_pan <- iniciativas_pan %>% 
  unlist() %>% 
  as_tibble()

iniciativas_prd <- iniciativas_prd %>% 
  unlist() %>% 
  as_tibble()

iniciativas_pvem <- iniciativas_pvem %>% 
  unlist() %>% 
  as_tibble()

iniciativas_pt <- iniciativas_pt %>% 
  unlist() %>% 
  as_tibble()


iniciativas_panal <- iniciativas_panal %>% 
  unlist() %>% 
  as_tibble()


iniciativas_mc <- iniciativas_mc %>% 
  unlist() %>% 
  as_tibble()

iniciativas_morena <- iniciativas_morena %>% 
  unlist() %>% 
  as_tibble()

iniciativas_pes <- iniciativas_pes %>% 
  unlist() %>% 
  as_tibble()

iniciativas_sp <- iniciativas_sp %>% 
  unlist() %>% 
  as_tibble()

### Unir todos ----

iniciativas_LXIII <- rbind(iniciativas_pri, iniciativas_pan, iniciativas_prd, iniciativas_panal, iniciativas_pvem, 
      iniciativas_mc, iniciativas_morena, iniciativas_pes, iniciativas_pt, iniciativas_sp)

iniciativas_LXIII <- iniciativas_LXIII %>% 
  count(value) %>% 
  mutate(prueba = str_detect(value, "Sen.")) %>% 
  filter(prueba == F,
         value != "PRESENTADA POR",
         value != "",
         value != " MC") %>% 
  rename(nom_dip = "value") %>% 
  select(-prueba) %>% 
  mutate(nom_dip = str_remove_all(nom_dip, "Dip. "),
         nom_dip = str_remove_all(nom_dip, "PAN"),
         nom_dip = str_remove_all(nom_dip, "PRI"),
         nom_dip = str_remove_all(nom_dip, "PRD"),
         nom_dip = str_remove_all(nom_dip, "PVEM"),
         nom_dip = str_remove_all(nom_dip, "PT"),
         nom_dip = str_remove_all(nom_dip, "MC"),
         nom_dip = str_remove_all(nom_dip, "PT"),
         nom_dip = str_remove_all(nom_dip, "Morena"),
         nom_dip = str_remove_all(nom_dip, "PES")) %>% 
  filter(nom_dip!=" ")

write_excel_csv(iniciativas_LXIII, "num_iniciativas_dip_2015.csv")
  
  
### Unir datos a bd ----

num_iniciativas_dip_2015 <- read_csv("num_iniciativas_dip_2015.csv")  
  

num_iniciativas_dip_2015 <- num_iniciativas_dip_2015 %>% 
  rename(nombre = "nom_dip")

nombre_diputados_2015 <- read_excel("C:/Users/helio/OneDrive - Centro de Investigacion y Docencia Economicas CIDE/7mo semestre/investigacion/01_documentos/nombre_diputados_2015.xlsx")


nombre_diputados_2015 <- nombre_diputados_2015 %>% 
  select(nombre)


prueba_1 <- left_join(nombre_diputados_2015, num_iniciativas_dip_2015) %>% 
  mutate(prueba = is.na(n)) %>% 
  filter(prueba == F) %>%
  group_by(nombre) %>% 
  summarise(numero = sum(n))
  

prueba_num_iniciativas_presentadas_2015 <- left_join(nombre_diputados_2015, prueba_1)

write_excel_csv(prueba_num_iniciativas_presentadas_2015, "prueba_num_iniciativas_presentadas_2015.csv")
