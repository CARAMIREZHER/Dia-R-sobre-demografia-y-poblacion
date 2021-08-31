## Dia R - Analisis poblacional eventos naturales
## Autora: Juliana Guerrero Velásquez
## Julio 14 2021

# remover objetos
rm(list = ls())


# librerias
library(data.table)
library(tidyverse)

# directorio
directorio <- "C:/Users/Data/Censo"
setwd(directorio)

# nombres y codigos de departamentos
departamento <- c("Antioquia","Atlantico","Bogota","Bolivar","Boyaca","Caldas","Caqueta","Cauca","Cesar","Cordoba","Cundinamarca","Choco","Huila","LaGuajira","Magdalena","Meta","Narino","NorteDeSantander","Quindio","Risaralda","Santander","Sucre","Tolima","ValleDelCauca","Arauca","Casanare","Putumayo","SanAndresProvidenciaYSantaCatalina","Amazonas","Guainia","Guaviare","Vaupes","Vichada")
cod          <- c("05","08","11","13","15","17","18","19","20","23","25","27","41","44","47","50","52","54","63","66","68","70","73","76","81","85","86","88","91","94","95","97","99")



# cod encuestas expuestas en amenaza - insumo DANE
inu = read.csv("cruce_censo.csv",header=T,
               colClasses = c(COD_ENCUESTAS="character",U_DPTO="character",U_MPIO="character"))



## Union viviendas censo 

# viviendas expuestas
viviendas_exp <- data.frame()
for(i in 1:33){
  print(i)
  vivienda <- fread(paste0(directorio,"/",cod[i],departamento[i],"/CNPV2018_1VIV_A2_",cod[i],".CSV"),
                    colClasses = c(COD_ENCUESTAS="character",U_DPTO="character",U_MPIO="character"))
  vivienda <- vivienda[vivienda$COD_ENCUESTAS %in% inu$COD_ENCUESTAS,]
  viviendas_exp <- rbind.data.frame(viviendas_exp,vivienda)
}

# viviendas no expuestas
viviendas_noexp <- data.frame()
for(i in 1:33){
  print(i)
  vivienda <- fread(paste0(directorio,"/",cod[i],departamento[i],"/CNPV2018_1VIV_A2_",cod[i],".CSV"),
                    colClasses = c(COD_ENCUESTAS="character",U_DPTO="character",U_MPIO="character"))
  vivienda <- vivienda[!(vivienda$COD_ENCUESTAS %in% inu$COD_ENCUESTAS),]
  viviendas_noexp <- rbind.data.frame(viviendas_noexp,vivienda)
}

# guardar resultados
saveRDS(viviendas_exp,"viviendas_exp_censo.rds")
saveRDS(viviendas_noexp,"viviendas_noexp_censo.rds")
# remover objetos
rm(viviendas_exp,viviendas_noexp)

## Union hogares 

# hogares expuestos
hogares_exp <- data.frame()
for(i in 1:33){
  print(i)
  hogar <- fread(paste0(directorio,"/",cod[i],departamento[i],"/CNPV2018_2HOG_A2_",cod[i],".CSV"),
                 colClasses = c(COD_ENCUESTAS="character",U_DPTO="character",U_MPIO="character"))
  hogar <- hogar[hogar$COD_ENCUESTAS %in% inu$COD_ENCUESTAS,]
  hogares_exp <- rbind.data.frame(hogares_exp,hogar)
}
# guardar resultado
saveRDS(hogares_exp,"hogares_exp_censo.rds")
rm(hogar,hogares_exp)

# hogares no expuestos
hogares_noexp <- data.frame()
for(i in 1:33){
  print(i)
  hogar <- fread(paste0(directorio,"/",cod[i],departamento[i],"/CNPV2018_2HOG_A2_",cod[i],".CSV"),
                 colClasses = c(COD_ENCUESTAS="character",U_DPTO="character",U_MPIO="character"))
  hogar <- hogar[!(hogar$COD_ENCUESTAS %in% inu$COD_ENCUESTAS),]
  hogares_noexp <- rbind.data.frame(hogares_noexp,hogar)
}
# guardar resultado
saveRDS(hogares_noexp,"hogares_noexp_censo.rds")
rm(hogares,vivienda,hogar,hogares_noexp)



## union personas censo

# personas expuestas
personas_exp <- data.frame()
for(i in 1:33){
  print(i)
  persona <- fread(paste0(directorio,"/",cod[i],departamento[i],"/CNPV2018_5PER_A2_",cod[i],".CSV"),
                   colClasses = c(COD_ENCUESTAS="character",U_DPTO="character",U_MPIO="character"))
  persona <- persona[persona$COD_ENCUESTAS %in% inu$COD_ENCUESTAS,]
  personas_exp <- rbind.data.frame(personas_exp,persona)
}
# guardar resultado
saveRDS(personas_exp,"personas_exp_censo.rds")
rm(persona,personas_exp)

# personas no expuestas
personas_noexp <- data.frame()
for(i in 1:33){
  print(i)
  persona <- fread(paste0(directorio,"/",cod[i],departamento[i],"/CNPV2018_5PER_A2_",cod[i],".CSV"),
                   colClasses = c(COD_ENCUESTAS="character",U_DPTO="character",U_MPIO="character"))
  persona <- persona[!(persona$COD_ENCUESTAS %in% inu$COD_ENCUESTAS),]
  personas_noexp <- rbind.data.frame(personas_noexp,persona)
}

# guardar resultado
saveRDS(personas_noexp,"personas_noexp_censo1.rds")
rm(persona,personas_noexp)


## lectura de dataset creados
viviendas_exp <- readRDS("viviendas_exp_censo.rds")
hogares_exp <- readRDS("hogares_exp_censo.rds")
personas_exp <- readRDS("personas_exp_censo.rds")
