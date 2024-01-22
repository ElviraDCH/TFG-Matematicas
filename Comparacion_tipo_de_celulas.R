library(survival)
library(KMsurv)
library(paletteer)

# Datos de los pscientes segun el tipo de celula cancerigena 
Large <-veteran[veteran$celltype =='large',]
Squamous<-veteran[veteran$celltype =='squamous',]
Smallcell <-veteran[veteran$celltype =='smallcell',]
Adeno <-veteran[veteran$celltype =='adeno',]


#Objetos Surv y Survfit para cada tipo de c??lula cancerigena mediante el estimador de Kaplan-Meier
Large.surv <- Surv(Large$time, Large$status)
Large.km <- survfit(Large.surv~ 1, data = Large, type = "kaplan-meier")

Squamous.surv <- Surv(Squamous$time, Squamous$status)
Squamous.km <- survfit(Squamous.surv~ 1, data = Squamous, type = "kaplan-meier")

Smallcell.surv <- Surv(Smallcell$time, Smallcell$status)
Smallcell.km <- survfit(Smallcell.surv~ 1, data = Smallcell, type = "kaplan-meier")

Adeno.surv <- Surv(Adeno$time, Adeno$status)
Adeno.km <- survfit(Adeno.surv~ 1, data = Adeno, type = "kaplan-meier")

#Resumenes de los datos de suervivencia de cada grupo 
summary(Large.km)
summary(Smallcell.km)
summary(Squamous.km)
summary(Adeno.km)

#Tiempos censurados para cada tipo de c??lula 
tiempo_Large <-182
censuras_Large <-0.3704

tiempo_Squamous <-c(25,87,100,231)
censuras_Squamous <-c(0.800,0.622,0.622,0.360)

tiempo_Smallcell <-c(97,103,123)
censuras_Smallcell <-c(0.2708,0.2257,0.1755)

tiempo_Adeno <- 83
censuras_Adeno <- 0.3704

#DataFrames
dfLarge <- data.frame(dx = Large.km$time, dy = Large.km$surv)
dfSquamous <- data.frame(dx1 = Squamous.km$time, dy1 = Squamous.km$surv)
dfSmallcell <- data.frame(dx2 = Smallcell.km$time, dy2 = Smallcell.km$surv)
dfAdeno <- data.frame(dx3 = Adeno.km$time, dy3 = Adeno.km$surv)

CensuraLarge<-data.frame(dx11 =tiempo_Large, dy11 =censuras_Large)
CensuraSmallcell<-data.frame(dx12 =tiempo_Smallcell, dy22 =censuras_Smallcell)
CensuraAdeno<-data.frame(dx13 =tiempo_Adeno, dy33 =censuras_Adeno)
CensuraSquamous<-data.frame(dx14 =tiempo_Squamous, dy44 =censuras_Squamous)

#Representacion de la estimacion de supervivencia para cada tipo de celula cancerigena con el m??todo de Kaplan-Meier
ggplot()+
  geom_step(data = dfLarge,aes(x=dx, y=dy, color='0',linetype = '0', shape = '0'),stroke=NA,size=1.1)+
  geom_step(data = dfSquamous,aes(x=dx1, y=dy1, color='3',linetype = '1', shape = '1'),stroke=NA,size=1.1)+
  geom_step(data = dfSmallcell,aes(x=dx2, y=dy2, color='2', linetype = '2', shape = '2'),stroke=NA,size=1.1)+
  geom_step(data = dfAdeno,aes(x=dx3, y=dy3, color='1', linetype = '3', shape = '3'),stroke=NA,size=1.1)+
  geom_point(data = CensuraLarge,aes(x=dx11, y=dy11, color='4',linetype = '4', shape = '4'),stroke= 1.2 ,size =1.5)+
  geom_point(data = CensuraSquamous,aes(x=dx14, y=dy44, color='7',linetype = '7', shape = '7'),stroke= 1.2 ,size =1.5)+
  geom_point(data = CensuraSmallcell,aes(x=dx12, y=dy22, color='6',linetype = '6', shape = '6'),stroke= 1.2 ,size =1.5)+
  geom_point(data = CensuraAdeno,aes(x=dx13, y=dy33, color='5',linetype = '5', shape = '5'),stroke= 1.2 ,size =1.5)+
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
  labs(x='Tiempo en d??as', y='Probabilidad de supervivencia')+
  scale_colour_manual(' ',values = c('0' = '#BD263E','1' ='#FFAE34','2' ='#6388B4FF', '3'='#55AD89', '4'='#D83842', '5'='#F3A546', '6'='#1170AA', '7'='#26897E'),
                      labels = c("Large","Adeno","Smallcell","Squamous", "Observaciones censuradas-Large", "Observaciones censuradas-Adeno",
                                 "Observaciones censuradas-Smallcell", "Observaciones censuradas-Squamous"))+
  scale_linetype_manual(' ', values = c('0'='solid', '1'= 'solid', '2'= 'solid', '3'='solid', '4'= NA, '5'=NA, '6'=NA, '7'=NA),
                        labels = c("Large","Adeno","Smallcell","Squamous", "Observaciones censuradas-Large", "Observaciones censuradas-Adeno",
                        "Observaciones censuradas-Smallcell", "Observaciones censuradas-Squamous"))+
  scale_shape_manual(' ', values = c('0'= NA, '1'= NA, '2'=NA, '3'= NA, '4'= 3, '5'= 3, '6'=3, '7'=3),
                     labels = c("Large","Adeno","Smallcell","Squamous", "Observaciones censuradas-Large", "Observaciones censuradas-Adeno",
                     "Observaciones censuradas-Smallcell", "Observaciones censuradas-Squamous"))
