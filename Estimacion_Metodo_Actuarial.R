library(tidyverse)
library(paletteer)
library(ggplot2)

# Estimación de la función de supervivencia mediante el método actuarial
S <- c(0.679, 0.515, 0.312, 0.211, 0.158, 0.088, 0.070, 0.035, 0.018, 0.018,
       0.018, 0,0)

# División de la duración del estudio en intervalos de 50 dias 
t <- seq(0,600,50)

# Valores censurados
cx <-c(25, 97, 100, 123, 182)
cy <-c(0.679,0.515,0.312,0.312,0.211)

# DataFrames
df <- data.frame(dx=t, dy=S)
dp <-data.frame(dx1=cx,dy1 =cy)

# Representación de la función de supervivencia estimada mediante el método actuarial 
ggplot()+
  geom_step(data = df,aes(x=dx, y=dy, color='0',linetype = '0', shape= '0'),stroke=NA,size=1.2)+
  geom_point(data = dp,aes(x=dx1, y=dy1, color='1',linetype = '1', shape = '1'),stroke= 1.2,size =1.7)+
  theme(panel.background = element_rect(fill = 'white'),
        panel.border = element_rect(color = "black", fill=NA, size = 1.5),
        axis.text = element_text(color = "black",size=18),
        axis.title.x = element_text(size=rel(2), margin = margin(t = 6, r=6, l =6)),
        axis.title.y = element_text(size=rel(2),margin = margin(r = 13)),
        legend.position = c(0.65, .77),
        legend.background = element_rect(fill = "white",colour = 1,size =0.9),
        legend.key = element_rect(fill = "white", color = NA),
        legend.text = element_text(size=18),
        legend.title = element_blank(),
        legend.spacing.y = unit(0.01, 'cm'))+
  labs(x='Tiempo en días', y='Probabilidad de supervivencia')+
  scale_colour_manual(' ',values = c('0' = '#BD263E','1' = 'black'),
                      labels = c( "Estimación actuarial", "Observaciones censuradas"))+
  scale_linetype_manual(' ', values = c('0'='solid', '1'= NA),
                        labels = c( "Estimación actuarial", "Observaciones censuradas"))+
  scale_shape_manual(' ', values = c('0'= NA, '1'= 3),
                     labels = c( "Estimación actuarial", "Observaciones censuradas"))
