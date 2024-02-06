library(lattice)
library(survival)
library(ggplot2)
library(survminer)
library(ggpubr)

#Datos de los pacientes con c??lulas cancer??genas tipo Large y Adeno
Large <-veteran[veteran$celltype =='large',]
Adeno <-veteran[veteran$celltype =='adeno',]

#Objeto Surv y funcion survfit para cada tipo de celula 
Large.surv <- Surv(Large$time, Large$status)
Adeno.surv <- Surv(Adeno$time, Adeno$status)

#Se calcula la funci??n de riesgo acumulado
mod1<-coxph(Large.surv~1, data=Large, method="breslow")
mod2<-coxph(Adeno.surv~1, data=Adeno, method="breslow")
bh1 <- basehaz(mod1)
bh2 <- basehaz(mod2)
lnhazard1<-log(bh1[,1])
lntime1<-log(bh1[,2])
lnhazard2<-log(bh2[,1])
lntime2<-log(bh2[,2])

#DataFrames
dfLarge <- data.frame(dx = lntime1, dy = lnhazard1)
dfAdeno <- data.frame(dx1 = lntime2, dy1 = lnhazard2)

#DataFrame UNICAMENTE para las celulas Large y Adeno
juntos <-merge(Large, Adeno, all = TRUE)

#Estimaci??n de los coeficientes del modelo
fitcomp <- coxph(Surv(time, status)~celltype, data=juntos)

#Verificaci??n de las hip??tesis de riesgo proporcionales
ftest <- cox.zph(fitcomp)

#p-valor para cada una de las covariables
p_valor <- ftest

#Gr??fica de los residuos de Schoenfeld
ggcoxzph(ftest)

#Representaci??n de las curvas de supervivencia log-log 
ggplot()+
  geom_line(data = dfLarge,aes(x=dx, y=dy, color='0'),size=1.1)+
  geom_line(data = dfAdeno,aes(x=dx1, y=dy1, color='1'),size=1.1)+
  theme(panel.background = element_rect(fill = 'white'),
        panel.border = element_rect(color = "black", fill=NA, size = 1.5),
        axis.text = element_text(color = "black",size=18),
        axis.title.x = element_text(size=rel(2), hjust=0.55, margin = margin(t = 7)),
        axis.title.y = element_text(size=rel(2),margin = margin(r = 13)),
        legend.position = c(0.35,.87),
        legend.background = element_rect(fill = "white",colour = 1,size =0.9),
        legend.key = element_rect(fill = "white"),
        legend.text = element_text(size=18), 
        legend.title = element_blank(),   
        legend.spacing.y = unit(0.01, 'cm'),
        legend.spacing.x = unit(0.02, 'cm'))+ 
  labs(x='ln(t)', y = 'ln(-ln(S(t)))')+
  scale_colour_manual(' ',values = c('0'='#BD263E','1'='#FFAE34'),
                      labels = c("Large","Adeno"))
  