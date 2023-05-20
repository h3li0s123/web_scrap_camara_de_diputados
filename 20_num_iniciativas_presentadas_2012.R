library(rvest)
library(tidyverse)
library(readxl)

### Iniciativas del PRI de 2012 ----

iniciativas_pri <- lapply(paste0("http://sil.gobernacion.gob.mx/Numeralia/Iniciativas/resultadosNumeraliaIniciativas.php?SID=&Origen=IL&Serial=e82075ae3de9a40b835929752525c943&Reg=705&Paginas=15&pagina=", 1:47),
                          function(url){
                            url %>% read_html() %>% 
                              html_nodes("td:nth-child(7)") %>% 
                              html_text() %>%
                              gsub('[\r\n\t]', '', .)
                            
                          })

iniciativas_pri <- iniciativas_pri %>%
  unlist() %>% 
  as_tibble()

### Iniciativas del PAN de 2012 ----

iniciativas_pan <- lapply(paste0("http://sil.gobernacion.gob.mx/Numeralia/Iniciativas/resultadosNumeraliaIniciativas.php?SID=&Origen=IL&Serial=aeb48fea8c7e9e918aa1a14d91837aa1&Reg=738&Paginas=15&pagina=", 1:50),
                          function(url){
                            url %>% read_html() %>% 
                              html_nodes("td:nth-child(7)") %>% 
                              html_text() %>%
                              gsub('[\r\n\t]', '', .)
                            
                          })

iniciativas_pan <- iniciativas_pan %>% 
  unlist() %>% 
  as_tibble()

### Iniciativas del PRD de 2012 ----

iniciativas_prd <- lapply(paste0("http://sil.gobernacion.gob.mx/Numeralia/Iniciativas/resultadosNumeraliaIniciativas.php?SID=&Origen=IL&Serial=66d7ed575fe97ebb76e8c896438f0bb4&Reg=606&Paginas=15&pagina=", 1:41),
                          function(url){
                            url %>% read_html() %>% 
                              html_nodes("td:nth-child(7)") %>% 
                              html_text() %>%
                              gsub('[\r\n\t]', '', .)
                            
                          })


iniciativas_prd <- iniciativas_prd %>% 
  unlist() %>% 
  as_tibble()

### Iniciativas del PVEM de 2012 ----

iniciativas_pvem <- lapply(paste0("http://sil.gobernacion.gob.mx/Numeralia/Iniciativas/resultadosNumeraliaIniciativas.php?SID=&Origen=IL&Serial=d714727cc75103297ae93c8e25e6aa66&Reg=238&Paginas=15&pagina=", 1:16),
                          function(url){
                            url %>% read_html() %>% 
                              html_nodes("td:nth-child(7)") %>% 
                              html_text() %>%
                              gsub('[\r\n\t]', '', .)
                            
                          })

iniciativas_pvem <- iniciativas_pvem %>% 
  unlist() %>% 
  as_tibble()


### Iniciativas del PT de 2012 ----

iniciativas_pt <- lapply(paste0("http://sil.gobernacion.gob.mx/Numeralia/Iniciativas/resultadosNumeraliaIniciativas.php?SID=&Origen=IL&Serial=4a67a85b6ca738b34f50f8d4ead37359&Reg=153&Paginas=15&pagina=", 1:11),
                           function(url){
                             url %>% read_html() %>% 
                               html_nodes("td:nth-child(7)") %>% 
                               html_text() %>%
                               gsub('[\r\n\t]', '', .)
                             
                           })

iniciativas_pt <- iniciativas_pt %>% 
  unlist() %>% 
  as_tibble()


### Iniciativas del PANAL de 2012 ----

iniciativas_panal <- lapply(paste0("http://sil.gobernacion.gob.mx/Numeralia/Iniciativas/resultadosNumeraliaIniciativas.php?SID=&Origen=IL&Serial=95c01e86e3ba481265ed23efcd93c6d4&Reg=242&Paginas=15&pagina=", 1:17),
                         function(url){
                           url %>% read_html() %>% 
                             html_nodes("td:nth-child(7)") %>% 
                             html_text() %>%
                             gsub('[\r\n\t]', '', .)
                           
                         })

iniciativas_panal <- iniciativas_panal %>% 
  unlist() %>% 
  as_tibble()

### Iniciativas del MC de 2012 ----

iniciativas_mc <- lapply(paste0("http://sil.gobernacion.gob.mx/Numeralia/Iniciativas/resultadosNumeraliaIniciativas.php?SID=&Origen=IL&Serial=ebd763be9b9e1d95e9a2225a03f6a8f6&Reg=500&Paginas=15&pagina=", 1:34),
                            function(url){
                              url %>% read_html() %>% 
                                html_nodes("td:nth-child(7)") %>% 
                                html_text() %>%
                                gsub('[\r\n\t]', '', .)
                              
                            })

iniciativas_mc <- iniciativas_mc %>% 
  unlist() %>% 
  as_tibble()


### Iniciativas del Morenea de 2012 ----

iniciativas_morena <- lapply(paste0("http://sil.gobernacion.gob.mx/Numeralia/Iniciativas/resultadosNumeraliaIniciativas.php?SID=&Serial=4f9853208a3f3d5aebd639b569415382&Reg=12&Origen=IL"),
                         function(url){
                           url %>% read_html() %>% 
                             html_nodes("td:nth-child(7)") %>% 
                             html_text() %>%
                             gsub('[\r\n\t]', '', .)
                           
                         })

iniciativas_morena <- iniciativas_morena %>% 
  unlist() %>% 
  as_tibble()

#### Unir todos ----

iniciativas_LXII <- rbind(iniciativas_pri, iniciativas_pan, iniciativas_prd, iniciativas_panal, iniciativas_pvem, 
                           iniciativas_mc, iniciativas_morena, iniciativas_pt)


iniciativas_LXII <-  iniciativas_LXII %>% 
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

write_excel_csv(iniciativas_LXII, "iniciativas_LXII.csv")

### Unir datos a bd ----

num_iniciativas_dip_2012 <- read_csv("iniciativas_LXII.csv")  

nombre_diputados_2012 <- read_excel("C:/Users/helio/OneDrive - Centro de Investigacion y Docencia Economicas CIDE/7mo semestre/investigacion/01_documentos/base_diputados_2012.xlsx") %>% 
  select(nombre)

num_iniciativas_dip_2012 <- num_iniciativas_dip_2012 %>% 
  rename(nombre = "nom_dip")


iniciativas_presentadas_2012 <- left_join(nombre_diputados_2012, num_iniciativas_dip_2012)

write_excel_csv(iniciativas_presentadas_2012, "iniciativas_presentadas_2012.csv")
