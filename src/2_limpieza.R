### Limpieza ###

# Librerias ----
library(tidyverse)
library(lubridate)

# Lectura ----
source("./src/1_lectura.R")

# Limpieza ----

## Viajes ----
viajes_clean <- viajes_raw %>% 
  mutate(
    inicio = strptime(inicio_del_viaje,format = "%Y-%m-%d %H:%M:%S"),
    final = strptime(fin_del_viaje,format = "%Y-%m-%d %H:%M:%S"),
    duracion = final-inicio,
    ini_redond = as.POSIXct(floor_date(inicio, unit = "hour"),format="%Y-%m-%d %H:%M:%S"),
    horaini = hour(ini_redond),
    diaini = str_replace(wday(inicio,label = TRUE, abbr = TRUE), "\\\\.", ""),
    fin_redond = as.POSIXct(floor_date(final, unit = "hour"),format="%Y-%m-%d %H:%M:%S"),
    horafin = hour(fin_redond),
    diafin = str_replace(wday(final,label = TRUE, abbr = TRUE), "\\\\.", ""))

df = viajes_clean

df2= df[!(df$origen_id==df$destino_id| df$duracion<300),] 

df3=df2%>% filter(duracion> 60, duracion < 21600) 


df4=df3%>% filter(horaini>4 | horaini >= 0 & horaini<1 ) 
df4=df4%>% filter(horafin>4 | horafin >= 0 & horafin<1 ) 

df5= na.omit(df4) 

df6 = df5%>% filter(inicio>="2022-11-07 00:00:00"& final<"2022-11-14 00:02:00")
viajes_clean = df6

# Cantidad de NAs
viajes_clean %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "Cantidad de NAs") # No hay NAs

## Estaciones infor ----
estaciones_info_clean <- estaciones_info_raw %>%
  filter(status != "NOT_IN_SERVICE")

# Salidas ----

# Cantidad de viajes por dia y hora de inicio
sal = viajes_clean %>%
  group_by(origen_id,diaini,horaini) %>% summarise(viajes = sum(viaje))

# Viajes totales por dia
dia = sal %>% group_by(origen_id, diaini) %>% 
  summarise(totviaj = sum(viajes))
sal = left_join(sal, dia)

# Proporción de viajes por día
sal$pc = sal$viajes/sal$totviaj
sal$tiemp = paste(sal$horaini,sal$diaini,"ini",sep="_")

sal = sal %>% ungroup()

sal = sal %>% select(origen_id,tiemp,pc)

sal_wide <- spread(sal, tiemp, pc)

sal_wide[is.na(sal_wide)] <- 0

# llegadas ----
llega = viajes_clean %>%
  group_by(destino_id,diafin,horafin)%>%   summarise(viajes = sum(viaje))

dia2 = llega %>% group_by(destino_id,diafin) %>% 
  summarise(totviaj = sum(viajes))
llega = left_join(llega,dia2)

llega$pc = llega$viajes/llega$totviaj
llega$tiemp = paste(llega$horafin,llega$diafin,"fin",sep="_")

llega = llega %>% ungroup()

llega = llega %>% select(destino_id,tiemp,pc)

llega_wide <- spread(llega, tiemp, pc)

llega_wide[is.na(llega_wide)] <- 0


# Merge

estaciones_clean = left_join(llega_wide, sal_wide, by = c("destino_id" = "origen_id"))

estaciones_clean = estaciones_clean[!(estaciones_clean$destino_id == '201'),]

# Limpieza viajes ----

viajes_clean <- viajes_raw %>%
  select(-viaje) %>%
  filter(origen_id != destino_id, 
         as_date(inicio_del_viaje) >= "2022-11-07",
         as_date(inicio_del_viaje) < "2022-11-14") %>%
  mutate(across(ends_with("viaje"), ~ as_datetime(.x)), # Se convierten a fechas - tiempo
         duracion_viaje = difftime(fin_del_viaje, inicio_del_viaje, units = "mins"),
         horario = ifelse(minute(inicio_del_viaje) >= 55 & duracion_viaje > 5, hour(fin_del_viaje), hour(inicio_del_viaje)), # Esta condición clasifica los viajes con el horario del final del viaje si el inicio del viaje está muy próximo al siguiente horario y este dura más de 5 mins.
         duracion_viaje_cut = factor(case_when(duracion_viaje <= 5 ~ "0 a 5 min",
                                               duracion_viaje > 5 & duracion_viaje <= 10 ~ "6 a 10 min",
                                               duracion_viaje > 10 & duracion_viaje <= 15 ~ "11 a 15 min",
                                               duracion_viaje > 15 & duracion_viaje <= 30 ~ "16 a 30 min",
                                               duracion_viaje > 30 & duracion_viaje <= 45 ~ "31 a 45 min",
                                               duracion_viaje > 45 & duracion_viaje <= 60 ~ "46 a 60 min", 
                                               duracion_viaje > 60 ~ "Más de 60 min"), levels = c("0 a 5 min", "6 a 10 min", "11 a 15 min", "16 a 30 min", "31 a 45 min", "46 a 60 min", "Más de 60 min")),
         genero = factor(genero, levels = c("M", "F", "NULL"), labels = c("Hombre", "Mujer", "NULL"))) %>%
  filter(duracion_viaje > 5 & duracion_viaje < 360 &
           hour(inicio_del_viaje) %in% c(0, seq(5, 23, 1)))
max(viajes_clean$inicio_del_viaje)
# Escritura
saveRDS(viajes_clean, "./data/3_clean/viajes_clean.RDS")
write_csv(estaciones_info_clean, "./data/3_clean/estaciones_info_clean.csv")
write_csv(estaciones_clean,"./data/3_clean/estacionhorapc.csv")