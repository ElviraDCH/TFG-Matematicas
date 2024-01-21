library(tidyverse)
library(paletteer)

#Parámetro de escala fijo (lambda = 0.7) y parámetro de forma variable (p)
t <- seq(0,3,0.1)
lambda <- 0.7 

# p = 2.2
h <-function(x){lambda*2.2*(x)^(2.2-1)}
S <- function(x){exp(-lambda*(x)^2.2)}
h_FIJA <-Vectorize(h)
S_FIJA <- Vectorize(S)

# p = 1.5
h1 <-function(x){lambda*1.5*(x)^(1.5-1)}
S1 <- function(x){exp(-lambda*(x)^1.5)}
h1_FIJA <-Vectorize(h1)
S1_FIJA <- Vectorize(S1)


# p = 1
h2 <-function(x){lambda*1*(x)^(1-1)}
S2 <- function(x){exp(-lambda*(x)^1)}
h2_FIJA <-Vectorize(h2)
S2_FIJA <- Vectorize(S2)

# p = 0.5
h3 <-function(x){lambda*0.5*(x)^(0.5-1)}
S3 <- function(x){exp(-lambda*(x)^0.5)}
h3_FIJA <-Vectorize(h3)
S3_FIJA <- Vectorize(S3)


# p = 0.2
h4 <-function(x){lambda*0.2*(x)^(0.2-1)}
S4 <- function(x){exp(-lambda*(x)^0.2)}
h4_FIJA <-Vectorize(h4)
S4_FIJA <- Vectorize(S4)


#Gráfica de las funciones de riesgo
ggplot(data =tibble(lims = t), aes(x=t))+
  theme(panel.background = element_rect(fill = 'white'),
        panel.border = element_rect(color = "black", fill=NA, size = 1.5),
        axis.text = element_text(color = "black",size=18),
        axis.title.x = element_text(size=rel(2)),
        axis.title.y = element_text(size=rel(2)),
        legend.position = c(.35, .77),
        legend.background = element_rect(fill = "white",colour = 1, size =0.9),
        legend.key = element_rect(fill = "white", color = NA),
        legend.text = element_text(size=18),
        legend.title = element_text(size = 19))+
  labs(x='Tiempo', y='  ')+
  stat_function(fun = h_FIJA, aes(color='0'), size =1.5)+
  stat_function(fun = h1_FIJA, aes(color='1'), size = 1.5)+
  stat_function(fun = h2_FIJA, aes(color='2'), size = 1.5)+
  stat_function(fun = h3_FIJA, aes(color='3'), size =1.5)+
  stat_function(fun = h4_FIJA, aes(color='4'), size =1.5)+
  scale_colour_manual('       Parámetros',values = c('0' = '#EF6F6A','1' ='#6388B4FF','2' ='#BD263E','3' ='#55AD89','4' = '#FFAE34'),
                      labels = c( expression("  "*lambda*" = 0.7   p = 2.2 "), expression(" "*lambda*" = 0.7   p = 1.5 "), expression(" "*lambda*" = 0.7   p = 1.0 "),
                                  expression("  "*lambda*" = 0.7   p = 0.5 "), expression(" "*lambda*" = 0.7   p = 0.2 ")))


#Gráfica de las funciones de supervivencia
ggplot(data =tibble(lims = t), aes(x=t))+
  theme(panel.background = element_rect(fill = 'white'),
        panel.border = element_rect(color = "black", fill=NA, size = 1.5),
        axis.text = element_text(color = "black",size=18),
        axis.title.x = element_text(size=rel(2)),
        axis.title.y = element_text(size=rel(2)),
        legend.position = c(.65, .77),
        legend.background = element_rect(fill = "white",colour = 1,size =0.9),
        legend.key = element_rect(fill = "white", color = NA),
        legend.text = element_text(size=18),
        legend.title = element_text(size = 19))+
  labs(x='Tiempo', y='  ')+
  stat_function(fun = S_FIJA, aes(color='0'), size=1.5)+
  stat_function(fun = S1_FIJA, aes(color='1'), size=1.5)+
  stat_function(fun = S2_FIJA, aes(color='2'), size=1.5)+
  stat_function(fun = S3_FIJA, aes(color='3'), size=1.5)+
  stat_function(fun = S4_FIJA, aes(color='4'), size=1.5)+
  scale_colour_manual('       Parámetros',values = c('0' = '#EF6F6A','1' ='#6388B4FF','2' ='#BD263E','3' ='#55AD89','4' = '#FFAE34'),
                      labels = c( expression("  "*lambda*" = 0.7   p = 2.2 "), expression(" "*lambda*" = 0.7   p = 1.5 "), expression(" "*lambda*" = 0.7   p = 1.0 "),
                                  expression("  "*lambda*" = 0.7   p = 0.5 "), expression(" "*lambda*" = 0.7   p = 0.2 ")))
