library(tidyverse)
library(survival)
library(KMsurv)
library(paletteer)

# Pacientes que recibieron el tratamiento estándar
Grupo_1 <-veteran[veteran$trt!='2',] 

# Pacientes que recibieron el tratamiento de quimioterapia 
Grupo_2 <-veteran[veteran$trt!='1',] 


# Objetos Surv y Survfit para cada grupo de tratamiento mediante el estimador de Kaplan-Meier
Grupo1.surv <-Surv(Grupo_1$time,Grupo_1$status)
Grupo1.km <-survfit(Grupo1.surv ~ 1, data = Grupo_1, type ="kaplan-meier")

Grupo2.surv <- Surv(Grupo_2$time, Grupo_2$status) 
Grupo2.km <- survfit(Grupo2.surv ~ 1, data = Grupo_2, type = "kaplan-meier")

# Tablas resumen de la estimación de la función de supervivencia (S) para cada grupo de tratamiento 
summary(Grupo1.km)
summary(Grupo2.km)

# Datos censurados del grupo 1-tratamiento estándar
censuras_KM <-c(0.7536,0.5172,0.5020,0.3922,0.2124)
censuras_tiempo <-c(25, 97,100, 123,182)

# Datos censurados del grupo 2-tratamiento de quimioterapia
censuras_KM2 <-c(0.4265,0.3960,0.3326,0.1830)
censuras_tiempo2 <-c(83,87,103,231)


# DataFrames
df <-data.frame(dx =Grupo1.km$time, dy = Grupo1.km$surv)
dc <-data.frame(dx1 =censuras_tiempo, dy1 =censuras_KM )
IC1 <-data.frame(d1 = Grupo1.km$time, d2 =Grupo1.km$upper, d4 =Grupo1.km$lower)

df2 <-data.frame(dx2 =Grupo2.km$time, dy2 = Grupo2.km$surv)
dc2 <-data.frame(dx12 =censuras_tiempo2, dy12 =censuras_KM2 )
IC2 <-data.frame(d12 = Grupo2.km$time, d22 =Grupo2.km$upper, d42 =Grupo2.km$lower)

# Representación de la estimación de supervivencia para cada grupo de tratamiento 
ggplot()+
  geom_step(data = df,aes(x=dx, y=dy, color='0',linetype = '0', shape= '0', fill = '0'),stroke=NA,size=1.1)+
  geom_step(data = df2,aes(x=dx2, y=dy2, color='1',linetype = '1', shape= '1', fill = '1'),stroke=NA,size=1.1)+
  geom_ribbon(data =IC1, aes(x=d1,ymin = d4,ymax =d2,color = '2',linetype = '2', shape = '2', fill ='2'),stroke= NA,size =1.5, alpha = 0.2)+
  geom_ribbon(data =IC2, aes(x=d12,ymin = d42,ymax =d22,color = '3',linetype = '3', shape = '3', fill = '3'),stroke= NA,size =1.5, alpha = 0.3)+
  geom_point(data = dc,aes(x=dx1, y=dy1, color='4',linetype = '4', shape = '4', fill = '4'),stroke= 1.2 ,size =1.5)+
  geom_point(data = dc2,aes(x=dx12, y=dy12, color='5',linetype = '5', shape = '5', fill = '5'),stroke= 1.2 ,size =1.5)+
  theme(panel.background = element_rect(fill = 'white'),
        panel.border = element_rect(color = "black", fill=NA, size = 1.5),
        axis.text = element_text(color = "black",size=18),
        axis.title.x = element_text(size=rel(2), hjust=0.55, margin = margin(t = 7)),
        axis.title.y = element_text(size=rel(2),margin = margin(r = 13)),
        legend.position = c(0.60, .77),
        legend.background = element_rect(fill = "white",colour = 1,size =0.9),
        legend.key = element_rect(fill = "white"),
        legend.text = element_text(size=18),
        legend.title = element_blank(),   
        legend.spacing.y = unit(0.01, 'cm'),
        legend.spacing.x = unit(0.02, 'cm'))+
  labs(x='Tiempo en días', y='Probabilidad de supervivencia')+
  scale_colour_manual(' ',values = c('0'='#BD263E','1'= '#166A2F','2' ='#A3123A', '3'='#68A768', '4'='black', '5'='#55AD89'),
                      labels = c("KM-Grupo 1","KM-Grupo 2","Intervalo de confianza-Grupo 1","Intervalo de confianza-Grupo 2","Observaciones censuradas-Grupo 1",
                                 "Observaciones censuradas-Grupo 2"))+
  scale_linetype_manual(' ', values = c('0'='solid', '1'= 'solid', '2'=NA, '3'=NA, '4'= NA, '5'=NA),
                        labels = c("KM-Grupo 1","KM-Grupo 2","Intervalo de confianza-Grupo 1","Intervalo de confianza-Grupo 2","Observaciones censuradas-Grupo 1",
                                   "Observaciones censuradas-Grupo 2"))+
  scale_shape_manual(' ', values = c('0'= NA, '1'= NA, '2'=NA, '3'= NA, '4'= 3, '5'= 3),
                     labels = c("KM-Grupo 1","KM-Grupo 2","Intervalo de confianza-Grupo 1","Intervalo de confianza-Grupo 2","Observaciones censuradas-Grupo 1",
                                "Observaciones censuradas-Grupo 2"))+
  scale_fill_manual(' ', values = c('0'='white', '1'= 'white', '2'= '#A3123A', '3'='#68A768', '4'='white', '5'= 'white'),
                    labels = c("KM-Grupo 1","KM-Grupo 2","Intervalo de confianza-Grupo 1","Intervalo de confianza-Grupo 2","Observaciones censuradas-Grupo 1",
                               "Observaciones censuradas-Grupo 2"))
