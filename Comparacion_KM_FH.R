library(survival)
library(KMsurv)
library(paletteer)

#Datos de los pacientes que han recibido el tratamiento estándar
Grupo_1 <-veteran[veteran$trt!='2',] 

Grupo1.surv <-Surv(Grupo_1$time,Grupo_1$status)
Grupo1.km <-survfit(Grupo1.surv ~ 1, data = Grupo_1, type ="kaplan-meier")
Grupo1.fh <-survfit(Grupo1.surv ~ 1, data = Grupo_1, type ="fleming-harrington")

#Valores censurados
censuras_KM <-c(0.7536,0.5172,0.5020,0.3922,0.2124)
censuras_tiempo <-c(25, 97,100, 123,182)

censuras_tiempo <-c(25, 97,100, 123,182)
censuras_FH <-c(0.75621,0.52155, 0.50644, 0.39786, 0.22018)

#DataFrames
df_km <-data.frame(dx =Grupo1.km$time, dy = Grupo1.km$surv)
dc_km<-data.frame(dx1 =censuras_tiempo, dy1 =censuras_KM )
IC1_km <-data.frame(d1 = Grupo1.km$time, d2 =Grupo1.km$upper, d4 =Grupo1.km$lower)

df_fh <-data.frame(dx2 =Grupo1.fh$time, dy2 = Grupo1.fh$surv)
dc_fh<-data.frame(dx12 =censuras_tiempo, dy12 =censuras_FH )
IC1_fh <-data.frame(d12 = Grupo1.fh$time, d22 =Grupo1.fh$upper, d42 =Grupo1.fh$lower)


#Representación de las funciones de supervivencia estimadas mediante el método de Kaplan-Meier y de Fleming-Harrington
ggplot()+
  geom_step(data = df_km,aes(x=dx, y=dy, color='0',linetype = '0', shape= '0', fill = '0'),stroke=NA,size=1.1)+
  geom_step(data = df_fh,aes(x=dx2, y=dy2, color='1',linetype = '1', shape= '1',fill = '1'),stroke=NA,size=1.1)+
  geom_ribbon(data =IC1_km, aes(x=d1,ymin = d4,ymax =d2,color = '2',linetype = '2', shape = '2', fill = '2'),stroke= NA,size =1.5, alpha = 0.2)+
  geom_ribbon(data =IC1_fh, aes(x=d12,ymin = d42,ymax =d22,color = '3',linetype = '3', shape = '3', fill = '3'),stroke= NA,size =1.5, alpha = 0.3)+
  geom_point(data = dc_km,aes(x=dx1, y=dy1, color='4',linetype = '4', shape = '4', fill= '4'),stroke= 1.2,size =1.5)+
  geom_point(data = dc_fh,aes(x=dx12, y=dy12, color='5',linetype = '5', shape = '5', fill = '5'),stroke= 1.2,size =1.5)+
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
  scale_colour_manual(' ',values = c('0'='#BD263E','1' = '#6388B4FF', '2' = '#A3123A', '3' = '#78A8CE', '4' = 'black', '5' = '#1F74B1'),
                      labels = c("Estimación KM","Estimación FH", "Intervalo de confianza KM", "Intervalo de confianza FH", 
                                 "Observaciones censuradas KM", "Observaciones censuradas FH"))+
  scale_linetype_manual(' ', values =c('0'='solid', '1'= 'solid', '2'=NA, '3'=NA, '4'= NA, '5'=NA),
                        labels = c("Estimación KM","Estimación FH", "Intervalo de confianza KM", "Intervalo de confianza FH", 
                                   "Observaciones censuradas KM", "Observaciones censuradas FH"))+
  scale_shape_manual(' ', values = c('0'= NA, '1'= NA, '2'=NA, '3'= NA, '4'= 3, '5'= 3),
                     labels = c("Estimación KM","Estimación FH", "Intervalo de confianza KM", "Intervalo de confianza FH", 
                                "Observaciones censuradas KM", "Observaciones censuradas FH"))+
  scale_fill_manual(' ', values = c('0'='white', '1'= 'white', '2'= '#A3123A', '3'= '#78A8CE', '4'= 'white', '5'= 'white'),
                    labels = c("Estimación KM","Estimación FH", "Intervalo de confianza KM", "Intervalo de confianza FH", 
                               "Observaciones censuradas KM", "Observaciones censuradas FH"))
 
