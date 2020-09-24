
#Tarea 1 

#Luciana Wainer 
library(sf) # Abrir bases de datos geograficas
library(leaflet) # Visualización interactiva (mapas)
library(tidyverse) # Manejo de bases de datos
library(htmlwidgets) # Para guardar paginas HTML
library(webshot)
library(plotly)
library(scales)


### Setup ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen=999)

# Una gráfica de ggplot 

#Base de datos elaboradas a partir de solicitudes de información. Cantidad de carpetas de investigación abiertas por estado por el delito de violencia digital (ley olimpia) 

library(readxl)
olimpia <- read_excel("~/Desktop/Luli/CIDE/Semestre 3/Estadística/Tareas/01_datos/olimpia.xlsx")


olimpia %>%
  filter(estado == c("BAJA CALIFORNIA", "CAMPECHE", "CHIAPAS", "CIUDAD DE MÉXICO", "GUANAJUATO", "NUEVO LEON", "OAXACA", "PUEBLA", "QUERETARO DE ARTEAGA", "SONORA", "ZACATECAS")) %>%
  ggplot() +
  aes(x = estado, y = carpetas) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(title = "Carpetas de investigación abiertas por violencia digital", caption = "Fuente: solicitudes de información, elaboración propia", x = "", y = "") +
  theme(plot.title = element_text(family = "Arial"), panel.background = element_rect(fill = "white")) 

# Un mapa estático en ggplot 
#Carpetas de investigación abiertas por estado

edos <- st_read("https://raw.githubusercontent.com/JuveCampos/MexicoSinIslas/master/mexicoTrends.geojson") %>% 
  st_transform(crs = 4326) 



plot(edos, max.plot = 1)

mapa_estado <- merge(x = edos, 
                     y = olimpia,
                     by.x = "ENTIDAD", 
                     by.y = "estado", 
                     all.y = "TRUE")

plot(mapa_estado, max.plot = 1)

mapa_estado %>%
  ggplot(aes(fill = carpetas)) +
  geom_sf() +
  scale_fill_gradientn(colors = c("#ffffff", "red")) + 
  labs(title = "Carpetas de investigación abiertas por violencia digital",
       subtitle = "Enero 2020", 
       caption = "Fuente: Solicitudes de información") +
  theme(panel.background = element_rect(fill = "black"), panel.grid.major = element_line(colour = "black")) + 
  theme(axis.text = element_blank()) +
   theme(axis.ticks = element_blank())

#Una gráfica en plotly
#Porcentaje de mujeres que presentaron una denuncia a lo largo de su vida por violencia de género

violencia <- read_csv("01_datos/Violencia_ayuda 2.csv", 
                              skip = 1)

violencia %>%
  filter(Entidad == "Nacional") %>%
  ggplot() +
  aes(x = Año, y = `%_mujeres` ) +
  scale_y_continuous(limits = c(10, 25)) +
      geom_line(color = "white") + 
  geom_point(color = "white") +
  labs(title = "Porcentaje de mujeres que denuncian violencia de género en todo México", subtitle = "Entre 2006 y 2016", caption = "Fuente: InMujeres", y = "", x = "") +
  theme_classic() +
  theme(panel.background = element_rect(fill = "black"))

ggplotly()


# Un mapa leaflet 
#Mapa de las oficinas del InMujeres en la Ciudad de México

inmujeres <- st_read("01_datos/INMUJERES_Unidades_Delegacionales.geojson")

mpios <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/mpios2.geojson", quiet = TRUE)

mpios <- mpios %>% 
  filter(CVE_ENT == "09")

cdmx <- edos %>%
  filter(ENTIDAD == "CIUDAD DE MÉXICO")

st_crs(inmujeres)
st_crs(mpios)
st_crs(cdmx)

oficinas <- inmujeres %>% 
  st_transform(crs = 4326)

st_crs(oficinas)

leaflet(options = leafletOptions(zoomControl = F)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = cdmx, color = "black", fill = NA) %>%
  addPolygons(data = mpios, color = "grey", fill = NA) %>%
  addCircleMarkers(data = inmujeres, color = "purple", radius = 1.5, popup = paste0("<b> Of. </b>", oficinas$UNIDAD_INM, "<br>", "<b> Direc. </b>", oficinas$DIRECCION,"<br>", "<b> Tel. </b>", oficinas$TELEFONOS))



#Una tabla
#Comparación de arpetas de investigación abiertas por estado, vinculaciones a proceso y sentencias al 20 de enero de 2020

| Estado             | Carpetas de investigación    | Vinculaciones a proceso | Sentencias |
  |-------------------|-------------|---------------|--------------|
  | Aguascalientes   | 0                             | 0                       | 0        | 
  | Baja California  |62                             | 0            | 0        |
  | B. California S. | 0                             | 0             | 0            |
  | Campeche         | 10                            | 0             | 0            |
  | Chiapas          | 12                            | 0             | 0            |
  | CDMX             | 38                            | 1             | 0            |
  | Guanajuato       | 99                            | 0             | 0            |
  | Nuevo León       | 40                            | 1             | 0            |
  | Oaxaca           | 5                             | 0             | 0            |
  | Puebla           | 19                            | 0             | 0            |
  | Querétaro        | 84                            | 0             | 0            |
  | Sonora           | 25                            | 0             | 0            |
  | Coahuila         | 32                            | 1             | 1 -3 años-   |
  |                  |                               |               |              |
  | TOTAL            | 426                           | 3             | 0            |