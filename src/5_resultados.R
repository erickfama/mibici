### Resultados ###

# Librerías ----
library(tidyverse)
library(ggtext)

# Lectura ----
clusters <- read_csv("./data/3_clean/estaciones_clusters.csv")

# Datos para LabSom
write_csv(clusters %>% select(-cluster), "./data/3_clean/clusters_labsom.csv")

# Formato grafico
th_col <- theme(plot.title = element_markdown(size = 14, hjust = 0.5),
                plot.subtitle = element_markdown(size = 12),
                axis.text.x = element_text(size = 12),
                axis.title.x = element_text(size = 12),
                axis.text.y = element_text(size = 12),
                axis.title.y = element_text(size = 12),
                legend.text = element_text(size = 12),
                legend.title = element_text(size = 13),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank()
)

# Clusters ----

# to long
clusters_long <- clusters %>%
  pivot_longer(-c(destino_id, cluster), names_to = "fecha_horario_tipo", values_to = "prop")

# Tasa promedio de salidas por cluster y horario
clusters_tasaMedia = clusters_long %>% 
  mutate(tipo = str_extract(fecha_horario_tipo, "ini|fin$"),
         hora = str_extract(fecha_horario_tipo, "^[0-9]+"),
         dia = str_extract(fecha_horario_tipo, "(?<=_)([a-zá-ü]+)(?=_)")) %>%
  group_by(cluster, tipo, hora, dia) %>% 
  summarise(prom = mean(prop)) %>%
  mutate(tipo = factor(tipo, levels = c("ini", "fin"), 
                       labels = c("Inicio", "Fin")),
         hora = factor(hora, levels = c("0", as.character(seq(5, 23, 1)))),
         dia = factor(dia, levels = c("lun", "mar", "mié", "jue", "vie", "sáb", "dom"), 
                      labels = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")))

# Escritura
saveRDS(clusters_tasaMedia, "./data/3_clean/clusters_tasaMedia.RDS")

clusters_tasaMedia <- readRDS("./data/3_clean/clusters_tasaMedia.RDS")

# Grafico de salidas por horario y dia
clusters_tasaMedia %>%
  filter(tipo == "Inicio") %>%
  ggplot(aes(hora, prom, group = as.factor(cluster), color = as.factor(cluster))) + 
  geom_line() + 
  scale_color_manual(values = c("green","orange","purple","blue","red","cyan", "pink"), name = "Cluster") + 
  scale_x_discrete(breaks = c("0", "7", "8", "9", "17", "18", "19", "23"), guide = guide_axis(n.dodge = 2)) + 
  facet_wrap(~dia, nrow = 2, ncol = 4) + 
  theme_bw() + 
  labs(title = "Distribución de la tasa promedio de <b><span style='color:#6666ff;'>salidas</span></b> por cluster",
       x = "Horario",
       y = "Tasa de salidas",
       caption = "Elaboración propia.") +
  th_col
  
  ggsave("./figs/2_resultados_salidas.png", plot = last_plot(), width = 10, height = 6, units = "in", dpi = 600)

# Grafico de llegadas por horario y dia
clusters_tasaMedia %>%
  filter(tipo == "Fin") %>%
  ggplot(aes(hora, prom, group = as.factor(cluster), color = as.factor(cluster))) + 
  geom_line() + 
  scale_color_manual(values = c("green","orange","purple","blue","red","cyan", "pink"), name = "Cluster") + 
  scale_x_discrete(breaks = c("0", "7", "8", "9", "17", "18", "19", "23"), guide = guide_axis(n.dodge = 2)) + 
  facet_wrap(~dia, nrow = 2, ncol = 4) + 
  theme_bw() + 
  labs(title = "Distribución de la tasa promedio de <b><span style='color:#ff6666;'>llegadas</span></b> por cluster",
       x = "Horario",
       y = "Tasa de salidas", 
       caption = "Elaboración propia.") + 
  th_col

  ggsave("./figs/2_resultados_llegadas.png", plot = last_plot(), width = 10, height = 6, units = "in", dpi = 600)


