### EDA ###

# Librerias ----
library(tidyverse)
library(lubridate)
library(sf)

# Lectura ---- 
estaciones_clean <- read_csv("./data/3_clean/estacionhorapc.csv")

estaciones_info <- read_csv("./data/3_clean/estaciones_info_clean.csv")

viajes_clean <- readRDS("./data/3_clean/viajes_clean.RDS") %>%
  mutate(dia = str_replace(wday(inicio_del_viaje, label = TRUE), "\\\\.", ""),
         dia = factor(dia, levels = c("lun", "mar", "mié", "jue", "vie", "sáb", "dom"), 
                      labels = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")))
# Formato para los graficos

th <- theme(plot.title = element_text(size = 14, hjust = 0.5),
            plot.subtitle = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      axis.title.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 13),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
)

# EDA ----

## Estaciones ---- 

# NAs
estaciones_clean %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "NA_sum") %>%
  ggplot(aes(NA_sum, variable)) +
  geom_bar(stat = "identity", col = "red", fill = "red", alpha = 0.3) + 
  scale_x_continuous(limits = c(0, nrow(estaciones_clean))) + 
  labs(title = "Cantidad de NAs en el conjunto de estaciones") + 
  theme_bw() # No hay NAs

### Distribucion geo de las estaciones ----
lel <- estaciones_info %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

jalisco <- st_read("./data/maps/jalisco/14mun.shp") %>% 
  st_transform(crs = 4326) %>%
  filter(NOMGEO %in% c("Guadalajara", "San Pedro Tlaquepaque", "Zapopan"))
 
jalisco %>%
  st_crop(ymin = 20.62, ymax = 20.75, xmin = -103.45, xmax = -103.2) %>%
  ggplot() +
  geom_sf(aes(fill = NOMGEO)) +
  scale_fill_manual(name = "Municipio", values = c("Guadalajara" = "#088da5", "San Pedro Tlaquepaque" = "#a5088d", "Zapopan" = "#8da508")) + 
  geom_sf(data = lel, color = "#2008a5") +
  labs(title = "Distribución de las estaciones del sistema MiBici en la ZMG", 
       subtitle = "Cantidad de estaciones = 300") +
  theme_bw() +
  th

ggsave("./figs/dist_estaciones.png", plot = last_plot(), width = 10, height = 6, units = "in", dpi = 600)


## Viajes ----

# NAs
viajes_clean %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "NA_sum") %>%
  ggplot(aes(NA_sum, variable)) +
  geom_bar(stat = "identity", col = "green", fill = "green", alpha = 0.3) + 
  scale_x_continuous(limits = c(0, 3000)) + 
  labs(title = "Cantidad de NAs en el conjunto de viajes",
       x = "NAs") + 
  theme_bw() + # NAs en la columna de año de nacimiento
  th

# Horarios más populares
viajes_clean %>%
  ggplot(aes(horario)) + 
  geom_histogram(bins = 24, color = "#fdc820", fill = "#00953B", size = 1.1) + 
  scale_x_continuous(breaks = seq(0, 23, 1)) +
  labs(title = "Distribución de los horarios",
       subtitle = glue::glue("Periodo: {as_date(min(viajes_clean$inicio_del_viaje))} : {as_date(max(viajes_clean$inicio_del_viaje))}"),
       y = "Cantidad de viajes",
       x = "Horario",
       caption = "Elaboración propia.") + 
  theme_bw() +
  th

  ggsave("./figs/dist_horarios.png", plot = last_plot(), width = 10, height = 6, units = "in", dpi = 600)

# Horarios más populares por día
viajes_clean %>%
  ggplot(aes(horario, fill = dia)) + 
  geom_histogram(bins = 24, color = "#fdc820", size = 1.1) + 
  scale_x_continuous(breaks = seq(0, 23, 1), guide = guide_axis(n.dodge = 2)) +
  scale_fill_discrete(guide = "none") +
  facet_wrap(~dia) + 
  labs(title = "Distribución de los horarios por día",
       subtitle = glue::glue("Periodo: {as_date(min(viajes_clean$inicio_del_viaje))} : {as_date(max(viajes_clean$inicio_del_viaje))}"),
       x = "",
       y = "Cantidad de viajes",
       caption = "Elaboración propia.") + 
  theme_bw() +
  th

  ggsave("./figs/dist_horarios_dia.png", plot = last_plot(), width = 10, height = 6, units = "in", dpi = 600)

  # Duración de los viajes
viajes_clean %>%
  ggplot(aes(duracion_viaje_cut)) +
  geom_histogram(stat = "count", color = "#fdc820", fill = "#00953B", size = 1.1) +
  scale_y_continuous(limits = c(0, nrow(viajes_clean)/2)) + 
  labs(title = "Distribución de la duración de los viajes",
       x = "",
       y = "Cantidad de viajes",
       caption = "Elaboración propia.") +
  theme_bw() + 
  th

  ggsave("./figs/dist_duracion_viajes.png", plot = last_plot(), width = 10, height = 6, units = "in", dpi = 600)
  
# Duración de los viajes por día
  viajes_clean %>%
  ggplot(aes(duracion_viaje_cut, fill = dia)) +
  geom_histogram(stat = "count", color = "#fdc820", size = 1) +
  scale_y_continuous(limits = c(0, 7500)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_fill_discrete(guide = "none") +
  facet_wrap(~dia) + 
  labs(title = "Distribución de la duración de los viajes",
       x = "",
       y = "Cantidad de viajes",
       caption = "Elaboración propia.") +
  theme_bw() + 
  th

  ggsave("./figs/dist_duracion_viajes_dia.png", plot = last_plot(), width = 10, height = 6, units = "in", dpi = 600)

# Bicis ocupadas por estación en horarios de 8 am y 6 pm
# viajes_clean %>%
#   filter(horario %in% c(7, 8, 9, 17, 18, 19)) %>%
#   group_by(origen_id, horario, duracion_viaje_cut) %>%
#   summarise(puertos = n())
