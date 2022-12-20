
##librerías
library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)

df <- read_excel("datos_abiertos_2022_nov.xlsx")
#View(df)

colnames(df)= c("viaje_id","usuario_id",
                "genero","año_de_nacimiento",
                "inicio_del_viaje","fin_del_viaje",
                "origen_id","destino_id")

df$viaje=1 ##observacion

df=df%>% mutate(
  inicio = strptime(df$inicio_del_viaje,format = "%Y-%m-%d %H:%M:%S"),
  final = strptime(df$fin_del_viaje,format = "%Y-%m-%d %H:%M:%S"))

df=df%>% mutate(
  duracion = final-inicio,
  ini_redond = as.POSIXct(floor_date(df$inicio, unit = "hour"),format="%Y-%m-%d %H:%M:%S"),
  horaini = hour(ini_redond),
  diaini = wday(df$inicio,label = TRUE, abbr = TRUE),
  fin_redond = as.POSIXct(floor_date(df$final, unit = "hour"),format="%Y-%m-%d %H:%M:%S"),
  horafin = hour(fin_redond),
  diafin = wday(df$final,label = TRUE, abbr = TRUE))

df <- 
  df %>% 
  group_by(diaini)%>%
  mutate(diaini = case_when(
    diaini=="dom\\." ~ "dom",
    diaini=="lun\\." ~ "lun",
    diaini=="mar\\." ~ "mar",
    diaini=="mié\\." ~ "mie",
    diaini=="jue\\." ~ "jue",
    diaini=="vie\\." ~ "vie",
    diaini=="sáb\\." ~ "sab"))%>%
  ungroup()

df <- 
  df %>% 
  group_by(diafin)%>%
  mutate(diafin = case_when(
    diafin=="dom\\." ~ "dom",
    diafin=="lun\\." ~ "lun",
    diafin=="mar\\." ~ "mar",
    diafin=="mié\\." ~ "mie",
    diafin=="jue\\." ~ "jue",
    diafin=="vie\\." ~ "vie",
    diafin=="sáb\\." ~ "sab"))%>%
  ungroup()


##limpieza
df2=df[!(df$origen_id==df$destino_id| df$duracion<300),]

df3=df2%>% filter(duracion> 60, duracion <21600 )

unique(df4$horaini)

df4=df3%>% filter(horaini>4 | horaini >= 0 & horaini<1 )
df4=df4%>% filter(horafin>4 | horafin >= 0 & horafin<1 )

df5= na.omit(df4)


#salidas
sal = df5 %>%
  group_by(origen_id,diaini,horaini)%>%   summarise(viajes = sum(viaje))


dia=sal %>% group_by(origen_id,diaini) %>% 
  summarise(totviaj=sum(viajes))
sal=left_join(sal,dia)


sal$pc=sal$viajes/sal$totviaj
sal$tiemp=paste(sal$horaini,sal$diaini,"ini",sep="_")

sal=sal %>% ungroup()

sal=sal %>% select(origen_id,tiemp,pc)

sal_wide <- spread(sal, tiemp, pc)

sal_wide[is.na(sal_wide)] <- 0



#lleagadas
llega = df5 %>%
  group_by(destino_id,diafin,horafin)%>%   summarise(viajes = sum(viaje))


dia2=llega %>% group_by(destino_id,diafin) %>% 
  summarise(totviaj=sum(viajes))
llega=left_join(llega,dia2)


llega$pc=llega$viajes/llega$totviaj
llega$tiemp=paste(llega$horafin,llega$diafin,"fin",sep="_")


llega=llega %>% ungroup()

llega=llega %>% select(destino_id,tiemp,pc)

llega_wide <- spread(llega, tiemp, pc)

llega_wide[is.na(llega_wide)] <- 0


#merge

x=left_join(llega_wide,sal_wide, by=c("destino_id"="origen_id"))

x=x[!(x$destino_id == '201'),]


write.csv(x,"estacionhorapc.csv",row.names = FALSE)

##### PYTHON

#clusters
clusters <- read_csv("C:/Users/crist/Python CIDE/clusters.csv")

clusters=clusters[,-1]

clusterlong=gather(clusters, cluster,pc,2:281)

clusterselect=clusters %>% select(destino_id,cluster)


clusterlong$destino_id=as.factor(clusterlong$destino_id)

clusterselect$destino_id=as.factor(clusterselect$destino_id)

clusterss=left_join(clusterlong,clusterselect,by=c("destino_id"="destino_id"))

  
groupcluster= clusterss %>% group_by(cluster.y,cluster.x) %>% 
  summarise(prom=mean(pc))


clustergraf=groupcluster %>% 
  mutate(
    tipo=str_sub(cluster.x,-3,))

write.csv(clustergraf,"clustergraf.csv")

###EXCEL
clustergraf <- read_excel("clustergraf.xlsx",sheet = "Hoja1")

clustergraf$cluster.y=as.character(clustergraf$cluster.y)

lun=clustergraf %>% filter(dianum==1)
df4=df3%>% filter(horaini>4 | horaini >= 0 & horaini<1 )


ggplot(clustergraf,aes(...8, prom,group=cluster.y,color=as.factor(cluster.y))) +
  scale_color_manual(values = c("green","orange","purple","blue","red","cyan"))+
  geom_line()+
  xlab("Horario") +
  ylab("Tasa de salidas") +
  theme_minimal()

ggplot(clusterss,aes(cluster.x, pc,group=destino_id,color=as.factor(cluster.y))) +
  scale_color_manual(values = c("green","orange","purple","blue","red","cyan"))+
  geom_line()+
  xlab("Horario") +
  ylab("Tasa de salidas") +
  theme_minimal()
