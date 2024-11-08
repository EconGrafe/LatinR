### Llamando las librerias

pacman::p_load(tidyverse, ggfortify, janitor, readxl, corrplot, sf)

setwd('C:/Users/Zadquiel/Documents/GitHub/LatinR/')

### Llamando la data

# Data Pobreza
data_pobreza <- read_excel('data_pobreza.xlsx') %>% clean_names()

# Data Parroquias
parr <- st_read('map/Parroquias_Venezuela.shp') %>%
  filter(ESTADO == 'Distrito Capital') %>%
  mutate_at(c('PARROQUIA'), ~case_when(. == 'Sucre' ~ 'Sucre (DC)',
                                       . == 'Cariaco' ~ 'Caricuao',
                                       T ~ PARROQUIA)) %>%
  select(PARROQUIA, geometry)

# Data Municipios
munven <- st_read('map/Municipios_Venezuela.shp') %>%
  filter(ESTADO == 'Miranda' & MUNICIPIO == 'Sucre' | MUNICIPIO == 'Chacao' | MUNICIPIO == 'EL Hatillo' | MUNICIPIO == 'Baruta') %>%
  mutate(MUNICIPIO = if_else(MUNICIPIO == 'Sucre', 'Sucre (M)', MUNICIPIO)) %>%
  mutate(MUNICIPIO = if_else(MUNICIPIO == 'EL Hatillo', 'El Hatillo', MUNICIPIO)) %>%
  select(PARROQUIA = MUNICIPIO, geometry)

# Unificamos ambas datas para el mapa
data_map <- rbind(parr, munven) %>% clean_names() %>%
  left_join(select(data_pobreza, parroquia, pobreza_ine), by = 'parroquia')

# Grafico de coropleta
ggplot() + geom_sf(data = data_map, aes(fill = pobreza_ine, geometry = geometry), color = 'white') + theme_light() +
  geom_sf_label(data = data_map, aes(label = parroquia), size = 2) +
  labs(title = 'Gran Caracas', subtitle = 'Pobreza', caption = 'Fuente: Censo INE 2011', 
       x = '', y = '') + 
  theme(legend.title = element_blank(), 
        axis.text.y = element_blank(), 
        axis.text.x = element_blank(),
        plot.title = element_text(face = 'bold', hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) + 
  scale_fill_continuous(type = 'viridis')

# Prueba de la funcion

source('pca_function.R') ### Llamando la función

pca <- PCA(data_pobreza[, -2], 'parroquia')

# Correlograma

corrplot(pca$`Correlaciones (Coeficiente)`, method = 'circle',
         title = 'Correlaciones entre Factores',
         type = 'lower', 
         tl.col = 'black', 
         order = 'hclust') 

# Visualizacion de la funcion

print(pca)

summary(pca)

