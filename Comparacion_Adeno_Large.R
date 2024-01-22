library(survival)
library(KMsurv)
library(paletteer)

# Datos de los pacientes con células cancerígenas tipo Large y Adeno
Large <-veteran[veteran$celltype =='large',]
Adeno <-veteran[veteran$celltype =='adeno',]


# Objeto Surv y función survfit para cada tipo de célula 
Large.surv <- Surv(Large$time, Large$status)
Large.km <- survfit(Large.surv~ 1, data = Large, type = "kaplan-meier")

Adeno.surv <- Surv(Adeno$time, Adeno$status)
Adeno.km <- survfit(Adeno.surv~ 1, data = Adeno, type = "kaplan-meier")

# Tabla resumen de la estimación de la función de supervivencia para cada tipo de célula 
summary(Large.km)
summary(Adeno.km)

# Datos censurados para cada tipo de célula 
tiempo_Adeno <- 83
censuras_Adeno <- 0.3704

tiempo_Large <- 182
censuras_Large <-  0.3704

# DataFrames
dfLarge <- data.frame(dx = Large.km$time, dy = Large.km$surv)
ICLarge <-data.frame(dx1 = Large.km$time, dy1 = Large.km$upper, dz1 = Large.km$lower)
CensuraLarge<-data.frame(dx11 =tiempo_Large, dy11 =censuras_Large)

dfAdeno <- data.frame(dx3 = Adeno.km$time, dy3 = Adeno.km$surv)
ICAdeno <-data.frame(d3 = Adeno.km$time, d32 = Adeno.km$upper, d33 = Adeno.km$lower)
CensuraAdeno <-data.frame(dx31 =tiempo_Adeno, dy31 =censuras_Adeno)

# Representación gráfica de las estimaciones de la función de supervivencia para el tipo de célula Adeno y Large 
ggplot()+
  geom_step(data = dfLarge,aes(x=dx, y=dy, color='0',linetype = '0', shape= '0', fill = '0'),stroke=NA,size=1.1)+
  geom_step(data = dfAdeno,aes(x=dx3, y=dy3, color='1',linetype = '1', shape= '1', fill = '1'),stroke=NA,size=1.1)+
  geom_ribbon(data =ICLarge, aes(x=dx1,ymin = dy1,ymax =dz1,color = '2',linetype = '2', shape = '2', fill ='2'),stroke= NA,size =1.5, alpha = 0.2)+
  geom_ribbon(data =ICAdeno, aes(x=d3,ymin = d32,ymax =d33,color = '3',linetype = '3', shape = '3', fill = '3'),stroke= NA,size =1.5, alpha = 0.3)+
  geom_point(data = CensuraLarge,aes(x=dx11, y=dy11, color='4',linetype = '4', shape = '4', fill = '4'),stroke= 1.2 ,size =1.5)+
  geom_point(data = CensuraAdeno,aes(x=dx31, y=dy31, color='5',linetype = '5', shape = '5', fill = '5'),stroke= 1.2 ,size =1.5)+
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
        legend.spacing.y = unit(0.01, 'cm'),
        legend.spacing.x = unit(0.02, 'cm'))+ 
  labs(x='Tiempo en días', y='Probabilidad de supervivencia')+
  scale_colour_manual(' ',values = c('0'='#BD263E','1'='#FFAE34','2' ='#D83842', '3'='#F3A546', '4'='black', '5'='#CA5621'),
                      labels = c("Large","Adeno", "Intervalo de confianza-Large","Intervalo de confianza-Adeno", "Observaciones censuradas-Large",
                                 "Observaciones censuradas-Adeno"))+
  scale_linetype_manual(' ', values = c('0'='solid', '1'= 'solid', '2'=NA, '3'=NA, '4'= NA, '5'=NA),
                        labels = c("Large","Adeno", "Intervalo de confianza-Large","Intervalo de confianza-Adeno", "Observaciones censuradas-Large",
                                   "Observaciones censuradas-Adeno"))+
  scale_shape_manual(' ', values = c('0'= NA, '1'= NA, '2'=NA, '3'= NA, '4'= 3, '5'= 3),
                     labels = c("Large","Adeno", "Intervalo de confianza-Large","Intervalo de confianza-Adeno", "Observaciones censuradas-Large",
                                "Observaciones censuradas-Adeno"))+
  scale_fill_manual(' ', values = c('0'='white', '1'= 'white', '2'= '#D83842', '3'='#F3A546','4'='white', '5'= 'white'),
                    labels = c("Large","Adeno", "Intervalo de confianza-Large","Intervalo de confianza-Adeno", "Observaciones censuradas-Large",
                               "Observaciones censuradas-Adeno"))

# DataFrame conjunto de ambas células 
juntos <-merge(Large, Adeno, all = TRUE)

# Test Log-Rank para los tipos de células cancerígenas Adeno y Large 
p_valor <-survdiff(Surv(time, status) ~ celltype,data=juntos)
p_valor
