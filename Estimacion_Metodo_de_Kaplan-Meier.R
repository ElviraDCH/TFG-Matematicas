library(survival)
library(KMsurv)
library(paletteer)


# Datos de los pacientes de veteran que han recibido el tratamiento estándar
Grupo_1 <-veteran[veteran$trt!='2',] 

#Surv combina información sobre el tiempo y la censura de los pacientes
Grupo1.surv <-Surv(Grupo_1$time,Grupo_1$status)

#survfit calcula las curvas de supervivencia según el estimador elegido, en este caso el de Kaplan-Meier
Grupo1.km <-survfit(Grupo1.surv ~ 1, data = Grupo_1, type ="kaplan-meier")

#Tabla resumen de la estimación de la función de supervivencia (S)
summary(Grupo1.km)

#Valores de los tiempos censurados
censuras_KM <-c(0.7536,0.5172,0.5020,0.3922,0.2124)
censuras_tiempo <-c(25, 97,100, 123,182)

#DataFrames
df <-data.frame(dx =Grupo1.km$time, dy = Grupo1.km$surv)
dc <-data.frame(dx1 =censuras_tiempo, dy1 =censuras_KM )
IC1 <-data.frame(d1 = Grupo1.km$time, d2 =Grupo1.km$upper, d4 =Grupo1.km$lower)

#Gráfica de la función de supervivencia mediante el método de Kaplan-Meier
ggplot()+
  geom_step(data = df,aes(x=dx, y=dy, color='1',linetype = '1', shape= '1', fill = '1'),stroke=NA,size=1.1)+
  geom_point(data = dc,aes(x=dx1, y=dy1, color='2',linetype = '2', shape = '2', fill= '2'),stroke= 1.2,size =1.5)+
  geom_ribbon(data =IC1, aes(x=d1,ymin = d4,ymax =d2,color = '0',linetype = '0', shape = '0', fill = '0'),stroke= NA,size =1.5, alpha = 0.1)+
  theme(panel.background = element_rect(fill = 'white'),
        panel.border = element_rect(color = "black", fill=NA, size = 1.5),
        axis.text = element_text(color = "black",size=18),
        axis.title.x = element_text(size=rel(2), hjust=0.55, margin = margin(t = 7)),
        axis.title.y = element_text(size=rel(2),margin = margin(r = 13)),
        legend.position = c(0.65, .77),
        legend.background = element_rect(fill = "white",colour = 1,size =0.9),
        legend.key = element_rect(fill = "white"),
        legend.text = element_text(size=18),
        legend.title = element_blank(),                               
        legend.spacing.y = unit(0.01, 'cm'))+
  labs(x='Tiempo en días', y='Probabilidad de supervivencia')+ 
  scale_colour_manual(' ',values = c('0'='#A3123A','1' = '#BD263E','2' = 'black'),
                      labels = c("Intervalo de confianza","Estimación Kaplan-Meier", "Observaciones censuradas"))+
  scale_linetype_manual(' ', values = c('0'=NA,'1'='solid', '2'= NA),
                        labels = c("Intervalo de confianza","Estimación Kaplan-Meier", "Observaciones censuradas"))+
  scale_shape_manual(' ', values = c('0'=NA, '1'= NA, '2'= 3),
                     labels = c("Intervalo de confianza","Estimación Kaplan-Meier", "Observaciones censuradas"))+
  scale_fill_manual(' ', values = c('0'='#A3123A', '1'= 'white', '2'= 'white'),
                    labels = c("Intervalo de confianza","Estimación Kaplan-Meier", "Observaciones censuradas"))
 
