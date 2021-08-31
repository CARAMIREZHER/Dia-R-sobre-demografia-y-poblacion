## Dia R - Analisis poblacional eventos naturales
## Autora: Juliana Guerrero Velásquez
## Julio 14 2021


# remover objetos 
rm(list = ls())

# librerias
library(tidyverse)
library(sf)
library(tmap)
library(RColorBrewer)

# working directory
setwd("C:/Users/R-day/Output")


# personas expuestas
per <- readRDS("personas_exp_censo.rds")


# transformaciones
per <- per %>% mutate(edad_tx=case_when(P_EDADR==1~'0-4', # etiquetas en quinquenios
                                        P_EDADR==2~'5-9',
                                        P_EDADR==3~'10-14',
                                        P_EDADR==4~'15-19',
                                        P_EDADR==5~'20-24',
                                        P_EDADR==6~'25-29',
                                        P_EDADR==7~'30-34',
                                        P_EDADR==8~'35-39',
                                        P_EDADR==9~'40-44',
                                        P_EDADR==10~'45-49',
                                        P_EDADR==11~'50-54',
                                        P_EDADR==12~'55-59',
                                        P_EDADR==13~'60-64',
                                        P_EDADR==14~'65-69',
                                        P_EDADR==15~'70-74',
                                        P_EDADR==16~'75-79',
                                        P_EDADR==17~'80-84',
                                        P_EDADR==18~'85-89',
                                        P_EDADR==19~'90-94',
                                        P_EDADR==20~'95-99',
                                        P_EDADR==21~'100+'),
                      edad_tx2 = case_when(P_EDADR==1~'0-4', # etiquetas en grupos de interes
                                           P_EDADR==2~'5-9',
                                           P_EDADR==3~'10-14',
                                           P_EDADR==4~'15-19',
                                           P_EDADR %in% c(5:12)~'20-59',
                                           P_EDADR %in% c(13:21)~'60+'))  

# estadisticas edad
est_edad <- per %>% group_by(edad_tx2) %>%
  summarise(n=n()) %>%
  mutate(porcentaje=n*100/sum(n))%>%
  filter(edad_tx2 %in% c('0-4','5-9','10-14','15-19','60+'))

# estadisticas reconocimiento étnico
per <- per %>% mutate(etn = case_when(PA1_GRP_ETNIC==1~'Indígena',
                                      PA1_GRP_ETNIC==2~'Rrom',
                                      PA1_GRP_ETNIC==3~'Raizal',
                                      PA1_GRP_ETNIC==4~'Palenquero/a',
                                      PA1_GRP_ETNIC==5~'Afrocolombiano/a',
                                      PA1_GRP_ETNIC==6~'Ninguno',
                                      PA1_GRP_ETNIC==9~'SinInformacion',))

est_etn <- per %>% mutate(etn_g = ifelse(etn %in% c('Ninguno','SinInformacion'),'ninguno','pob_etnic')) %>%
  group_by(etn_g)%>%
  summarise(n=n()) %>%
  mutate(porcentaje=n*100/sum(n))

# poblacion con alguna dificultad
est_dif <- per %>% group_by(CONDICION_FISICA)%>% #Alguna dificultad en su vida diaria
  summarise(n=n()) %>%
  mutate(porcentaje=n*100/sum(n))

# Poblacion 5-19 años sin asistencia escolar 
est_sae <- per %>% filter(P_EDADR %in% c(2,3,4) & PA_ASISTENCIA==2) %>%
  summarise(porcentaje = n()*100/nrow(per))

# personas con ingresos en hogar
est_dep <- per %>% group_by(COD_ENCUESTAS) %>%
  filter(P_TRABAJO %in% c(1,3,5)) %>%
  summarise(per_ingre = n())
  


## Hogares
hog <- readRDS("hogares_exp_censo.rds")

# hacinamiento 
# mas de 3 personas por cuarto
est_hac <- hog %>% mutate(hac=ifelse(HA_TOT_PER/H_NRO_DORMIT>3,1,0))%>%
  group_by(hac)%>%
  summarise(n=n()) %>%
  mutate(porcentaje_hac=n*100/sum(n))

## Viviendas
viv <- readRDS("viviendas_exp_censo.rds")

# materiales paredes 
# pat paredes 6. Madera burda, tabla, tablón
# 7. Caña, esterilla, otros vegetales
# 8. Materiales de deshecho (Zinc, tela, cartón, latas, plásticos, otros)
# 9. No tiene paredes
est_mat_par <- viv %>% mutate(mat_par=ifelse(V_MAT_PARED %in% c(6,7,8,9),1,0)) %>%
  group_by(mat_par) %>%
  summarise(n=n()) %>%
  mutate(porcentaje_hac=n*100/sum(n))

# materiales piso 
# mat piso 5. Madera burda, tabla, tablón, otro vegetal
          # 6. Tierra, arena, barro
est_mat_pis <- viv %>% mutate(mat_pis=ifelse(V_MAT_PISO %in% c(5,6),1,0)) %>%
  group_by(mat_pis) %>%
  summarise(n=n()) %>%
  mutate(porcentaje_hac=n*100/sum(n))



## gráficas
# pirámide poblacional
# pirámide poblacional 
pir <- per %>% group_by(edad_tx,P_SEXO) %>% summarise(pob=n())
names(pir) <- c('grupo_edad','sexo','pob')
pir <- pir %>% mutate(sexo = case_when(sexo==1~'Hombre',sexo==2~'Mujer'))  

pir$pob <- pir$pob / sum(pir$pob) * 100

level_order <- c('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49',
                 '50-54','55-59','60-64','65-69','70-74','75-79','80-84','85-89','90-94','95-99','100+')

ggplot(pir, aes(x = factor(grupo_edad,level=level_order), fill = sexo,
                y = ifelse(test = sexo == 'Hombre',#"Hombres",
                           yes = -pob, no = pob))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = 6 * c(-1,1)) +
  theme(legend.position="bottom")+
  labs(title = "Pirámide población expuesta", x = "edad", y = "Porcentaje de población") +
  scale_colour_manual(values = c("darkorange1", "green3"),aesthetics = c("colour", "fill")) +
  coord_flip()

## Exposición Por departamento 
per_dpto <- per %>% group_by(U_DPTO) %>%
  summarise(p_exp=n())

pob_dpto <- read.csv('poblacion_dane_dpto.csv',sep=';',colClasses = c(DP="character"))

pob_dpto <- merge(pob_dpto,per_dpto,by.x="DP",by.y="U_DPTO",all.x=T) %>% 
  mutate_all(~replace(., is.na(.), 0))

# porcentaje pob expuesta
pob_dpto$por_exp <- pob_dpto$p_exp*100/pob_dpto$Total
pob_dpto[order(-pob_dpto$por_exp),]

# analisis vulnerabilidad
# relacion exposicion-NBI
ggplot(pob_dpto, aes(x= por_exp, y= NBI, label=DPNOM)) +
  geom_point(size = 2,alpha = 0.5,show.legend=FALSE) +
  geom_text(aes(label=DPNOM),hjust=0.1, vjust=-2)+
  labs(y="%personas en NBI", x = "%Personas expuestas")

ggplot(aes(x=por_exp, y=NBI,label=DPNOM),data=pob_dpto) +
  geom_point(aes(size = p_exp), alpha = 0.5, show.legend = TRUE) +
  geom_text(nudge_y = 0.05,angle= 0,vjust= -1) +
  scale_size_continuous(range = c(1, 15), name="Total personas expuestas")+
  labs(y="%personas en NBI", x = "%Personas expuestas")


# capacidad de respuesta
# relacion exposicion-deficit habitacional cuanti
ggplot(pob_dpto, aes(x= por_exp, y= deficit_hab_cuanti, label=DPNOM)) +
  geom_point(size = 2,alpha = 0.6,show.legend=FALSE) +
  geom_text(aes(label=DPNOM),hjust=0.1, vjust=-2)+
  labs(y="%personas en déficit habitacional cuantitativo", x = "%Personas expuestas")

ggplot(aes(x=por_exp, y=deficit_hab_cuanti,label=DPNOM),data=pob_dpto) +
  geom_point(aes(size = p_exp), alpha = 0.5, show.legend = TRUE) +
  geom_text(nudge_y = 0.05,angle= 0,vjust= -1) +
  scale_size_continuous(range = c(1, 15), name="Total personas expuestas")+
  labs(y="%personas en déficit habitacional cuantitativo", x = "Personas expuestas por cada 1000 habitantes")


## Exposición por municipio
pob_mpio <- read.csv('poblacion_dane_mpio.csv',sep=';',colClasses = c(cod_dpto="character",cod_mpio="character"))

per_mpio <- per %>% mutate(cod_mpio=paste(U_DPTO,U_MPIO,sep='')) %>%group_by(cod_mpio) %>%
  summarise(p_exp=n())
            

pob_mpio <- merge(pob_mpio,per_mpio,by.x="cod_mpio",by.y="cod_mpio",all.x=T) %>% 
  mutate_all(~replace(., is.na(.), 0))

# porcentaje pob expuesta
pob_mpio$por_exp <- pob_mpio$p_exp*100/pob_mpio$pob
head(pob_mpio[order(-pob_mpio$por_exp),])

## mapas exposicion
# cargar shapes 
dpto_sh <- read_sf("shapes/MGN_DPTO_POLITICO.shp")


# pegar informacion de dpto
dpto_sh <- merge(dpto_sh,pob_dpto,by.x='DPTO_CCDGO',by.y='DP',all.x=T)

paleta <- "YlOrRd"

# mapa poblacion expuesta por dpto
m1 <- tm_shape(dpto_sh) + tm_polygons("p_exp", title = "Población expuesta", palette = paleta) + tm_layout(asp = 0)
m1

# mapa porcentaje pob expuesta por dpto
m2 <- tm_shape(dpto_sh) + tm_polygons("por_exp", title = "% Población expuesta", palette = paleta) + tm_layout(asp = 0)
m2

# mapa de municipios
mpio_sh <- read_sf("shapes/MGN_MPIO_POLITICO.shp")


# pegar informacion de dpto
mpio_sh <- merge(mpio_sh,pob_mpio,by.x='MPIO_CCNCT',by.y='cod_mpio',all.x=T)

# mapa poblacion expuesta por mpio
m3 <- tm_shape(mpio_sh) + tm_polygons("p_exp", title = "Población expuesta", palette = paleta) + tm_layout(asp = 0)
m3

  # mapa porcentaje pob expuesta por mpio

m4 <- tm_shape(mpio_sh) + tm_polygons("por_exp", title = "% Población expuesta", palette = paleta) + tm_layout(asp = 0)
m4

## Definición de vulnerabilidad
# dpto
dpto_sh <- dpto_sh %>% mutate(exp_c = case_when(por_exp>=30~'Alta',por_exp<=10~'Baja',TRUE~'Media'),
                              nbi_c = case_when(NBI>=60~'Alta',NBI<=30~'Baja',TRUE~'Media'),
                              dh_c = case_when(deficit_hab_cuanti>=60~'Alta',deficit_hab_cuanti<=30~'Baja',TRUE~'Media'),)

# vulnerabilidad
dpto_sh <- dpto_sh %>% mutate(vul_soec = case_when((exp_c=='Alta' & nbi_c=='Alta')~'Alta',
                                                   (exp_c=='Alta' & nbi_c=='Media')~'Alta',
                                                   (exp_c=='Alta' & nbi_c=='Baja')~'Media',
                                                   (exp_c=='Media' & nbi_c=='Alta')~'Alta',
                                                   (exp_c=='Media' & nbi_c=='Media')~'Media',
                                                   (exp_c=='Media' & nbi_c=='Baja')~'Media',TRUE~'Baja'))
# capacidad de respuesta
dpto_sh <- dpto_sh %>% mutate(vul_fis = case_when((exp_c=='Alta' & dh_c=='Alta')~'Alta',
                                                   (exp_c=='Alta' & dh_c=='Media')~'Alta',
                                                   (exp_c=='Alta' & dh_c=='Baja')~'Media',
                                                   (exp_c=='Media' & dh_c=='Alta')~'Alta',
                                                   (exp_c=='Media' & dh_c=='Media')~'Media',
                                                   (exp_c=='Media' & dh_c=='Baja')~'Media',TRUE~'Baja'))
# mpio 
mpio_sh <- mpio_sh %>% mutate(exp_c = case_when(por_exp>=30~'Alta',por_exp<=10~'Baja',TRUE~'Media'),
                              nbi_c = case_when(NBI>=60~'Alta',NBI<=30~'Baja',TRUE~'Media'),
                              dh_c = case_when(def_hab_cuanti>=60~'Alta',def_hab_cuanti<=30~'Baja',TRUE~'Media'),)

mpio_sh <- mpio_sh %>% mutate(vul_soec = case_when((exp_c=='Alta' & nbi_c=='Alta')~'Alta',
                                                   (exp_c=='Alta' & nbi_c=='Media')~'Alta',
                                                   (exp_c=='Alta' & nbi_c=='Baja')~'Media',
                                                   (exp_c=='Media' & nbi_c=='Alta')~'Alta',
                                                   (exp_c=='Media' & nbi_c=='Media')~'Media',
                                                   (exp_c=='Media' & nbi_c=='Baja')~'Media',TRUE~'Baja'),
                              vul_fis = case_when((exp_c=='Alta' & dh_c=='Alta')~'Alta',
                                                  (exp_c=='Alta' & dh_c=='Media')~'Alta',
                                                  (exp_c=='Alta' & dh_c=='Baja')~'Media',
                                                  (exp_c=='Media' & dh_c=='Alta')~'Alta',
                                                  (exp_c=='Media' & dh_c=='Media')~'Media',
                                                  (exp_c=='Media' & dh_c=='Baja')~'Media',TRUE~'Baja'))




# vulnerabilidad 
# socieconomica departamento
m5 <- tm_shape(dpto_sh) + tm_polygons("vul_soec", title = "Vulnerabilidad Socieconomica",
                                      palette = c('red','yellow','orange'))+ tm_layout(asp = 0)
m5

# socieconomica municipio
m6 <- tm_shape(mpio_sh) + tm_polygons("vul_soec", title = "Vulnerabilidad Socieconomica", 
                                      palette = c('red','yellow','orange')) + tm_layout(asp = 0)
m6

# fisica departamento
m7 <- tm_shape(dpto_sh) + tm_polygons("vul_fis", title = "Vulnerabilidad Física",
                                      palette = c('red','yellow','orange'))+ tm_layout(asp = 0)
m7

# fisica municipio
m8 <- tm_shape(mpio_sh) + tm_polygons("vul_fis", title = "Vulnerabilidad Física", 
                                      palette = c('red','yellow','orange')) + tm_layout(asp = 0)
m8
                              