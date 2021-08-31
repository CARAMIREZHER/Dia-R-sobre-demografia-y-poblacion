## Dia R - Analisis poblacional eventos naturales
## Autora: Juliana Guerrero Velásquez
## Julio 14 2021

# remover objetos
rm(list = ls())
# librerias
library(tidyverse)

# working directory
setwd("C:/Users/R-day/Output")

## lectura de dataset creados
viviendas_exp <- readRDS("viviendas_exp_censo.rds")
hogares_exp <- readRDS("hogares_exp_censo.rds")
personas_exp <- readRDS("personas_exp_censo.rds")

# nombres y codigos dptos
departamento <- c("Antioquia","Atlantico","Bogota","Bolivar","Boyaca","Caldas","Caqueta","Cauca","Cesar","Cordoba",
                  "Cundinamarca","Choco","Huila","LaGuajira","Magdalena","Meta","Narino","NorteDeSantander","Quindio","Risaralda","Santander","Sucre","Tolima","ValleDelCauca","Arauca","Casanare","Putumayo","SanAndresProvidenciaYSantaCatalina","Amazonas","Guainia","Guaviare","Vaupes","Vichada")
cod          <- c("05","08","11","13","15","17","18","19","20","23","25","27","41","44","47","50","52","54","63","66","68","70","73","76","81","85","86","88","91","94","95","97","99")
cods_dpto <- as.data.frame(cbind(cod,departamento))


# pegar nombres departamentos
viviendas_exp <- viviendas_exp %>% left_join(cods_dpto, by=c('U_DPTO'='cod'))

hogares_exp <- hogares_exp %>% left_join(cods_dpto, by=c('U_DPTO'='cod'))

personas_exp <- personas_exp %>% left_join(cods_dpto, by=c('U_DPTO'='cod'))


# graficos exploracion
# viviendas expuestas
viviendas_exp %>% group_by(departamento) %>% summarise(n=n()) %>%
  ggplot(aes(y=n,x=reorder(departamento,(n))))+
  geom_bar(position="dodge", stat="identity",fill='steelblue')+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  labs(y= "Viviendas expuestas", x = "Departamento")+
  coord_flip()

# hogares expuestos
hogares_exp %>% group_by(departamento) %>% summarise(n=n()) %>%
  ggplot(aes(y=n,x=reorder(departamento,(n))))+
  geom_bar(position="dodge", stat="identity",fill='darkorange2')+
  theme_bw()+
  scale_y_continuous(labels = comma)+
  labs(y= "Hogares expuestos", x = "Departamento")+
  coord_flip()

# personas expuestas
personas_exp %>% group_by(departamento) %>% summarise(n=n()) %>%
  ggplot(aes(y=n,x=reorder(departamento,(n))))+
  geom_bar(position="dodge", stat="identity",fill='darkgreen')+
  theme_bw()+
  labs(y= "Personas expuestas", x = "Departamento")+
  coord_flip()
