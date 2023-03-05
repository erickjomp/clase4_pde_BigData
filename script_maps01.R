#### Configuraciones adicionales ####
rm(list = ls())
gc()

library(tidyverse)
library(sf)
library(geodata)
library(tmap)
library(ggrepel)

# directorios
wd <- list()

wd$inputs <- "01_inputs/"
wd$datasets <- file.path(wd$inputs, "datasets/")
wd$output <- "02_outputs/"

# cargamos objeto espacial
sf_peru1 <- gadm("PER",1, path = tempdir()) %>% st_as_sf()

# grafico 
tm_shape(sf_peru1) + tm_borders(col = "black")

tmap_save(filename = file.path(wd$output, "mapa01.pdf"),
          height = 200, units = "mm")

# nombres
sf_peru1$NAME_1

## mapa Tacna
sf_peru1 %>% dplyr::filter(NAME_1 == "Tacna") %>% 
tm_shape() + tm_borders(col = "black")

tmap_save(filename = file.path(wd$output, "mapa02_Tacna.pdf"),
          height = 200, units = "mm")

# mapa con etiquetas
sf_peru1 %>%
  tm_shape() + tm_text("NAME_1",) + tm_borders()


# con ggplot 
sf_centroids <- st_centroid(sf_peru1) %>% 
  mutate(x = map_dbl(geometry, 1),
         y = map_dbl(geometry, 2))
sf_paises <- gadm(c("BRA","CHL","BOL","ECU", "COL"),
                  level = 0,
                  path = tempdir(),resolution = 2) %>% st_as_sf()

ggplot(data = sf_peru1) +
  geom_sf(data = sf_paises, fill = "white") +
  geom_sf(col = "black", fill = "royalblue") +
  geom_text_repel(data = sf_centroids, 
                  mapping = aes(x, y, label = NAME_1)) +
  theme_bw() +
  labs(x = "Longitud", y = "Latitud") +
  coord_sf(xlim = st_bbox(sf_peru1)[c(1,3)],
           ylim = st_bbox(sf_peru1)[c(2,4)]) +
  theme(panel.background = element_rect(fill = "lightblue"))

ggsave(file.path(wd$output,"mapa_peru.pdf"),
       height = 200, units = "mm")  
