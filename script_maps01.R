#### Configuraciones adicionales ####
rm(list = ls())
gc()

library(tidyverse)
library(sf)
library(geodata)

# directorios
wd <- list()

wd$inputs <- "01_inputs/"
wd$datasets <- file.path(wd$inputs, "datasets/")
wd$output <- "02_outputs/"

# cargamos objeto espacial
sf_peru1 <- gadm("PER",1, path = tempdir()) %>% st_as_sf()
