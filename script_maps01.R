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
sf_peru1 <- gadm("PER", 1, path = tempdir()) %>% st_as_sf()

# grafico
tm_shape(sf_peru1) + tm_borders(col = "black")

tmap_save(
  filename = file.path(wd$output, "mapa01.pdf"),
  height = 200,
  units = "mm"
)

# nombres
sf_peru1$NAME_1

## mapa Tacna
sf_peru1 %>% dplyr::filter(NAME_1 == "Tacna") %>%
  tm_shape() + tm_borders(col = "black")

tmap_save(
  filename = file.path(wd$output, "mapa02_Tacna.pdf"),
  height = 200,
  units = "mm"
)

# mapa con etiquetas
sf_peru1 %>%
  tm_shape() + tm_text("NAME_1", ) + tm_borders()


# con ggplot
sf_centroids <- st_centroid(sf_peru1) %>%
  mutate(x = map_dbl(geometry, 1),
         y = map_dbl(geometry, 2))
sf_paises <- gadm(
  c("BRA", "CHL", "BOL", "ECU", "COL"),
  level = 0,
  path = tempdir(),
  resolution = 2
) %>% st_as_sf()
(
  plot_mapa <-
    ggplot(data = sf_peru1) +
    geom_sf(data = sf_paises, fill = "white") +
    geom_sf(col = "black", fill = "royalblue") +
    geom_text_repel(data = sf_centroids,
                    mapping = aes(x, y, label = NAME_1)) +
    theme_bw() +
    labs(x = "Longitud", y = "Latitud") +
    coord_sf(xlim = st_bbox(sf_peru1)[c(1, 3)],
             ylim = st_bbox(sf_peru1)[c(2, 4)]) +
    theme(panel.background = element_rect(fill = "lightblue"))
)

ggsave(file.path(wd$output, "mapa_peru.pdf"),
       height = 200,
       units = "mm")


#### DATA ####
df_povrate2016 <-
  read_csv(file.path(wd$datasets, "povrate2016.csv"))
df_educ2016 <- read_csv(file.path(wd$datasets, "educ2016.csv"))

df_povrate2016 %>% head
df_povrate2016 %>% names
df_educ2016 %>% names

sf_peru1 <-
  sf_peru1 %>%
  mutate(NOMBDEP = textclean::replace_non_ascii(toupper(NAME_1)))

sf_peru1 <- sf_peru1 %>%
  left_join(df_povrate) %>%
  left_join(df_educ2016)

#### grafico pobreza ####
ggplot(data = sf_peru1) +
  geom_sf(data = sf_paises, fill = "white") +
  geom_sf(col = "black", aes(fill = poor)) +
  scale_fill_continuous(type = "viridis") +
  geom_text_repel(data = sf_centroids,
                  mapping = aes(x, y, label = NAME_1)) +
  theme_bw() +
  labs(x = "Longitud",
       y = "Latitud",
       fill = "Pobreza",
       title = "Mapa de pobreza en el Perú (2016)") +
  coord_sf(xlim = st_bbox(sf_peru1)[c(1, 3)],
           ylim = st_bbox(sf_peru1)[c(2, 4)]) +
  theme(panel.background = element_rect(fill = "lightblue"))

ggsave(file.path(wd$output, "mapa_peru_pobreza.pdf"),
       height = 200,
       units = "mm")

# grafico pobreza
ggplot(data = sf_peru1) +
  geom_sf(data = sf_paises, fill = "white") +
  geom_sf(col = "black", aes(fill = educ)) +
  scale_fill_continuous(type = "viridis") +
  geom_text_repel(data = sf_centroids,
                  mapping = aes(x, y, label = NAME_1)) +
  theme_bw() +
  labs(x = "Longitud",
       y = "Latitud",
       fill = "Años de educación",
       title = "Años de educación en el Perú (2016)") +
  # coord_sf( clip = "on") + 
  coord_sf(xlim = st_bbox(sf_peru1)[c(1, 3)],
           ylim = st_bbox(sf_peru1)[c(2, 4)]) +
  theme(panel.background = element_rect(fill = "lightblue"))

ggsave(file.path(wd$output, "mapa_peru_educacion.pdf"),
       height = 200,
       units = "mm")



#### Educacion y pobreza en un mapa ####
sf_centroids <- sf_centroids %>% 
  mutate(NOMBDEP = sf_peru1$NOMBDEP) %>% 
  left_join(df_povrate2016) %>% 
  left_join(df_educ2016)

ggplot(data = sf_peru1) +
  geom_sf(data = sf_paises, fill = "white") +
  geom_sf(col = "black", aes(fill = educ)) +
  geom_sf(data =sf_centroids, aes(size = poor), 
          fill = "white",
          col = "grey15",shape = 21) +
  scale_fill_continuous(type = "viridis") +
  geom_text_repel(data = sf_centroids,
                  mapping = aes(x, y, label = NAME_1)) +
  theme_bw() +
  labs(x = "Longitud",
       y = "Latitud",
       fill = "Años de educación",
       size = "Pobreza",
       title = "Años de educación y tasa de pobreza en el Perú (2016)") +
  # coord_sf( clip = "on") + 
  coord_sf(xlim = st_bbox(sf_peru1)[c(1, 3)],
           ylim = st_bbox(sf_peru1)[c(2, 4)]) +
  theme(panel.background = element_rect(fill = "lightblue"))

ggsave(file.path(wd$output, "mapa_peru_educacion_pobreza.pdf"),
       height = 200,
       units = "mm")

