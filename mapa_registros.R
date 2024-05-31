setwd("C:/Users/curciom/Desktop/proyectos/TRANSPORTE")
################
#cargo librerias
library(tidyverse)
library(readxl)
library(stringr)
library(stringi)
library(sp)
library(sf)
###############
#cargo datasets
dep_shp <- "C:/Users/curciom/Desktop/proyectos/TRANSPORTE/shp_departamentos/datos_shp/provincias.shp"
departamentos <- st_read(dep_shp, promote_to_multi = FALSE)
registros <- read_xlsx("registros_no_encontrados.xlsx")
patentamientos_provincia <- read_xlsx("patentamientos_provincia.xlsx")
registros_coord <- read_xlsx("REGISTRO AUTOMOTOR.xlsx")

#limpio los datasets
#paso todo a minuscula y saco puntos
#SHP de coordenadas de los departamentos
departamentos$FNA = stri_trans_general(str = departamentos$FNA, id = "Latin-ASCII")
departamentos$FNA = tolower(departamentos$FNA)
#datos de los registros
names(registros)[names(registros) == "provincia_nombre"] <- "FNA" #renombro la columna a FNA
registros$FNA = stri_trans_general(str = registros$FNA, id = "Latin-ASCII")
registros$FNA = tolower(registros$FNA)
#datos de los patentamientos por provincia
patentamientos_provincia$Provincia = stri_trans_general(str = patentamientos_provincia$Provincia, id = "Latin-ASCII")
patentamientos_provincia$Provincia = tolower(patentamientos_provincia$Provincia)
names(patentamientos_provincia)[names(patentamientos_provincia) == "Provincia"] <- "FNA" #renombro la columna a FNA
#renombro las provincias para hacer el join 
departamentos <- departamentos %>%
  mutate(FNA = case_when(FNA == "ciudad autonoma de buenos aires"~"ciudad autonoma de bs.as.",
                         FNA == "provincia del neuquen"~"neuquen",
                         FNA == "provincia de san luis"~"san luis",
                         FNA == "provincia de santa fe"~"santa fe",
                         FNA == "provincia del chubut"~"chubut",
                         FNA == "provincia de mendoza"~"mendoza",
                         FNA == "provincia de entre rios"~"entre rios",
                         FNA == "provincia de san juan"~"san juan",
                         FNA == "provincia de jujuy"~"jujuy",
                         FNA == "provincia de santiago del estero"~"santiago del estero",
                         FNA == "provincia de rio negro"~"rio negro",
                         FNA == "provincia de corrientes"~"corrientes",
                         FNA == "provincia de misiones"~"misiones",
                         FNA == "provincia de salta"~"salta",
                         FNA == "provincia de cordoba"~"cordoba",
                         FNA == "provincia de tierra del fuego, antartida e islas del atlantico sur"~"tierra del fuego",
                         FNA == "provincia de buenos aires"~"buenos aires",
                         FNA == "provincia de la pampa"~"la pampa",
                         FNA == "provincia de catamarca"~"catamarca",
                         FNA == "provincia de tucuman"~"tucuman",
                         FNA == "provincia del chaco"~"chaco",
                         FNA == "provincia de formosa"~"formosa",
                         FNA == "provincia de santa cruz"~"santa cruz",
                         FNA == "provincia de la rioja"~"la rioja",
                         FNA == "provincia de cordoba"~"cordoba"))


#cuento los registros por provincia
registros_group <- registros %>%
  group_by(FNA) %>%
  count()
#uno los registros por provincia con las coordenadas
registros_join <- registros_group %>%
  left_join(departamentos, by = "FNA")
#creo una columna para graficar los registros por provincia
registros_join_2 <- registros_join %>%
  mutate(grupo = case_when(n >= 10 & n<20 ~ "menos de 20 registros",
                           n >=20 & n<75 ~ "20 a 75 registros",
                           n >=75 & n<175 ~"75 a 175 registros",
                           n >=175 ~ "mas de 175 registros"))
#lo ordeno para que sea mas facil de graficar
registros_join_2$grupo <- factor(registros_join_2$grupo, levels = c("mas de 175 registros"
                                                                    ,"75 a 175 registros"
                                                                    ,"20 a 75 registros"
                                                                    ,"menos de 20 registros"))

#uno los patentamientos por provincia
registros_join_2 <- registros_join_2 %>%
  left_join(patentamientos_provincia, by = "FNA")
#lo ordeno en subgrupos para que el grafico sea mas claro
registros_join_2 <- registros_join_2 %>%
  mutate(grupo_patentado = case_when(Patentamiento >= 0 & Patentamiento<9999 ~ "menos de 10000 patentamientos",
                           Patentamiento >=10000 & Patentamiento<39999 ~ "10000 a 40000 patentamientos",
                           Patentamiento >=40000 & Patentamiento<79999 ~"40000 a 80000 patentamientos",
                           Patentamiento >=80000 ~ "mas de 80000 patentamientos"))
#esto es para facilitar los graficos
registros_join_2$grupo_patentado <- factor(registros_join_2$grupo_patentado, levels = c("menos de 10000 patentamientos"
                                                                    ,"10000 a 40000 patentamientos"
                                                                    ,"40000 a 80000 patentamientos"
                                                                    ,"mas de 80000 patentamientos"))
#los registros tenian lat y lon en una sola columna, lo divido en dos asi puedo ubicarlos en el mapa
registros_coord <- registros_coord %>%
  mutate(`Coordenadas 2` = str_trim(`Coordenadas 2`)) %>%  # Remover espacios en blanco
  separate(`Coordenadas 2`, into = c("lat", "lon"), sep = ",", convert = TRUE)
#unifico todas las coordenadas de las provincias en formato MULTIPOLYGON en caso de querer hacer el mapa
#en plotly
registros_join_2$geometry = sf::st_cast(registros_join_2$geometry, "MULTIPOLYGON")
#graficos
#################################
#grafico de registros por provincia
registros_x_provincia <- ggplot() +
  geom_sf(data = registros_join_2$geometry,
          aes(fill = registros_join_2$grupo)) +
  coord_sf(xlim=c(-75,-50),ylim=c(-20, -55)) +
  scale_fill_discrete(type = c("#242C4F","#4A5475","#707D9B","#9696C2" ),
    name = "",
    guide = guide_legend(
      keyheight = unit(3, units = "mm"),
      keywidth = unit(12, units = "mm"),
      label.position = "top",
      title.position = "bottom",
    )) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "white")) +
  labs(
    title = "Registros seccionales por provincia",
    caption = "Fuente: DNRPA | Autor: Marco Curcio")

registros_x_provincia
#########################################
#grafico de ubicacion de los registros en relacion a la cantidad de patentados en el 2023
#tooltip en caso de que quiera hacerlo en plotly
registros_coord$tooltip <- paste("Registro:", registros_coord$denominacion, 
                                 "Direccion:", registros_coord$domicilio)
mapa_registros <- ggplot() +
  geom_sf(data = registros_join_2$geometry,
          aes(fill = registros_join_2$grupo_patentado)) +
  geom_point(data = registros_coord, aes(x = lon, y = lat), size = 0.7 
                                         , color = "#E7BA61") +
  coord_sf(xlim=c(-75,-50),ylim=c(-20, -55)) +
  scale_fill_discrete(type = c("#242C4F","#4A5475","#707D9B","#9696C2" ),
                      name = "",
                      guide = guide_legend(
                        keyheight = unit(3, units = "mm"),
                        keywidth = unit(12, units = "mm"),
                        label.position = "top",
                        title.position = "bottom",
                      )) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "white")) +
  labs(
    title = "Ubicación registros seccionales",
    caption = "Fuente: DNRPA | Autor: Marco Curcio")

mapa_registros
