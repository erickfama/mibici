# Librerías
library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)

# Lectura ----

# Viajes
viajes_raw <- read_excel("./data/1_raw/datos_abiertos_2022_nov.xlsx") %>%
  janitor::clean_names()

# Info de las estaciones
estaciones_info_raw <- read_csv("./data/1_raw/nomenclatura_2022_11.csv") %>%
  janitor::clean_names()

viajes_raw$viaje <- 1 ## observacion




