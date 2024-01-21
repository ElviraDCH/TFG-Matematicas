library(survival)
library(KMsurv)
library(paletteer)

# Datos de los pacientes que recibieron el tratamiento est??ndar
Grupo_1 <-veteran[veteran$trt =='2',] 

#Surv combina informaci??n sobre el tiempo y la censura de los pacientes
Grupo_1.surv<-Surv(Grupo_1$time, Grupo_1$status)

#survfit calcula las curvas de supervivencia seg??n el estimador indicado, en este caso el de Fleming-Harrington
Grupo_1.km <-survfit(Grupo_1.surv ~ 1, data = Grupo_1, type ="fleming-harrington")

#Resumen 
summary(Grupo_1.km)

#Valores censurados
censuras_tiempo <-c(25, 97,100, 123,182)
censuras_FH <-c( 0.7531, 0.3711,0.3402,0.2912,0.2413)

#DataFrames
df <-data.frame(dx =Grupo_1.km$time, dy = Grupo_1.km$surv)
dc <-data.frame(dx1 =censuras_tiempo, dy1 =censuras_FH )
IC1 <-data.frame(d1 = Grupo_1.km$time, d2 =Grupo_1.km$upper, d4 =Grupo_1.km$lower)


#Representaci??n de la funci??n de supervivencia estimada mediante el m??todo de Fleming-Harrington       
ggplot()+
  geom_step(data = df,aes(x=dx, y=dy, color='2',linetype = '2', shape= '2',fill = '2'),stroke=NA,size=1.1)+
  geom_point(data = dc,aes(x=dx1, y=dy1, color='1',linetype = '1', shape = '1', fill = '1'),stroke= 1.2,size =1.5)+
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
        legend.spacing.y = unit(0.005, 'cm'))+
  labs(x='Tiempo en d??as', y='Probabilidad de supervivencia')+
  scale_colour_manual(' ',values = c('0'= '#A3123A','1' = 'black','2' = '#BD263E'),
                      labels = c("Intervalo de confianza","Observaciones censuradas","Estimaci??n Fleming-Harrington"))+
  scale_linetype_manual(' ', values = c('0'=NA, '1'= NA,'2'='solid'),
                        labels = c("Intervalo de confianza","Observaciones censuradas","Estimaci??n Fleming-Harrington"))+
  scale_shape_manual(' ', values = c('0'=NA,'1'= 3,'2'= NA),
                     labels = c("Intervalo de confianza","Observaciones censuradas","Estimaci??n Fleming-Harrington"))+
  scale_fill_manual(' ', values = c('0'='#A3123A', '1'= 'white', '2'= 'white'),
                    labels = c("Intervalo de confianza","Observaciones censuradas","Estimaci??n Fleming-Harrington"))



