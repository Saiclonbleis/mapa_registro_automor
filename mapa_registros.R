setwd("C:/Users/curciom/Desktop/proyectos/TRANSPORTE")
################
#cargo librerias
library(tidyverse)
library(readxl)
library(stringr)
library(stringi)
library(leaflet)
library(sp)
library(sf)
###############
#cargo datasets
dep_shp <- "C:/Users/curciom/Desktop/proyectos/TRANSPORTE/shp_departamentos/datos_shp/provincias.shp"
departamentos <- st_read(dep_shp, promote_to_multi = FALSE)
registros <- read_xlsx("registros_no_encontrados.xlsx")

#preparo los datasets

departamentos$FNA = stri_trans_general(str = departamentos$FNA, id = "Latin-ASCII")
departamentos$FNA = tolower(departamentos$FNA)

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


registros$FNA = stri_trans_general(str = registros$FNA, id = "Latin-ASCII")
registros$FNA = tolower(registros$FNA)

names(registros)[names(registros) == "provincia_nombre"] <- "FNA"
registros_group <- registros %>%
  group_by(FNA) %>%
  count()
registros_join <- registros_group %>%
  left_join(departamentos, by = "FNA")

ggplot() +
  geom_sf(data = registros_join$geometry,
          aes(fill = registros_join$n)) +
  coord_sf(xlim=c(-80,-50),ylim=c(-20, -55)) +
  scale_fill_continuous(breaks = c(400,375,350,325,300,275,250,
                                   225,200,175,150,125,100,75,
                                   50,25,0),
    name = "",
    guide = guide_legend(
      keyheight = unit(3, units = "mm"),
      keywidth = unit(12, units = "mm"),
      label.position = "top",
      title.position = "bottom",
    )) +
  labs(
    title = "Registros seccionales por provincia",
    caption = "Fuente: DNRPA | Autor: Marco Curcio")
    