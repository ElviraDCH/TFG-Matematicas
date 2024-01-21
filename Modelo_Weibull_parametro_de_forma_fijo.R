library(tidyverse)
library(paletteer)

#Par??metro de escala variable (lambda) y parametro de forma fijo (p = 2.2)
t <- seq(0,3,0.1)

# lambda = 3.5
h_1 <-function(x){(3.5)*(2.2)*(x)^(2.2-1)}
S_1 <- function(x){exp(-(3.5)*(x)^2.2)}
h_1FIJA <-Vectorize(h_1)
S_1FIJA <- Vectorize(S_1)

# lambda = 2
h_2 <-function(x){2*(2.2)*(x)^(2.2-1)}
S_2 <- function(x){exp(-2*(x)^2.2)}
h_2FIJA <-Vectorize(h_2)
S_2FIJA <- Vectorize(S_2)

# lambda = 1
h_3 <-function(x){1*(2.2)*(x)^(2.2-1)}
S_3 <- function(x){exp(-1*(x)^2.2)}
h_3FIJA <-Vectorize(h_3)
S_3FIJA <- Vectorize(S_3)

# lambda = 0.5
h_4 <-function(x){(0.5)*(2.2)*(x)^(2.2-1)}
S_4 <- function(x){exp(-(0.5)*(x)^2.2)}
h_4FIJA <-Vectorize(h_4)
S_4FIJA <- Vectorize(S_4)

# lambda = 0.3
h_5 <-function(x){(0.3)*(2.2)*(x)^(2.2-1)}
S_5 <- function(x){exp(-(0.3)*(x)^2.2)}
h_5FIJA <-Vectorize(h_5)
S_5FIJA <- Vectorize(S_5)


#Grafica de las funciones de riesgo
ggplot(data =tibble(lims = t), aes(x=t))+
  theme(panel.background = element_rect(fill = 'white'),
        panel.border = element_rect(color = "black", fill=NA, size = 1.5),
        axis.text = element_text(color = "black",size=18),
        axis.title.x = element_text(size=rel(2)),
        axis.title.y = element_text(size=rel(2)),
        legend.position = c(.35, .77),
        legend.background = element_rect(fill = "white",colour = 1, size =0.8),
        legend.key = element_rect(fill = "white", color = NA),
        legend.text = element_text(size=18),
        legend.title = element_text(size = 19))+
  labs(x='Tiempo', y='  ')+
  stat_function(fun = h_1FIJA, aes(color='l0'), size=1.5)+
  stat_function(fun = h_2FIJA, aes(color='l1'), size=1.5)+
  stat_function(fun = h_3FIJA, aes(color='l2'), size=1.5)+
  stat_function(fun = h_4FIJA, aes(color='l3'), size=1.5)+
  stat_function(fun = h_5FIJA, aes(color='l4'), size=1.5)+
  scale_colour_manual('       Par??metros',values = c('l0' = '#EF6F6A','l1' ='#6388B4FF','l2' ='#BD263E','l3' ='#55AD89','l4' = '#FFAE34'),
                      labels = c( expression("  "*lambda*" = 3.5   p = 2.2 "), expression(" "*lambda*" = 2.0   p = 2.2 "), expression(" "*lambda*" = 1.0   p = 2.2 "),
                                  expression("  "*lambda*" = 0.5   p = 2.2 "), expression(" "*lambda*" = 0.3   p = 2.2 ")))


#Grafica de las funciones de supervivencia
ggplot(data =tibble(lims = t), aes(x=t))+
  theme(panel.background = element_rect(fill = 'white'),
        panel.border = element_rect(color = "black", fill=NA, size = 1.5),
        axis.text = element_text(color = "black",size=18),
        axis.title.x = element_text(size=rel(2)),
        axis.title.y = element_text(size=rel(2)),
        legend.position = c(.75, .77),
        legend.background = element_rect(fill = "white",colour = 1, size =0.8),
        legend.key = element_rect(fill = "white", color = NA),
        legend.text = element_text(size=18),
        legend.title = element_text(size = 19))+
  labs(x='Tiempo', y='  ')+
  stat_function(fun = S_1FIJA, aes(color='l0'), size=1.5)+
  stat_function(fun = S_2FIJA, aes(color='l1'), size=1.5)+
  stat_function(fun = S_3FIJA, aes(color='l2'), size=1.5)+
  stat_function(fun = S_4FIJA, aes(color='l3'), size=1.5)+
  stat_function(fun = S_5FIJA, aes(color='l4'), size=1.5)+
  scale_colour_manual('       Par??metros',values = c('l0' = '#EF6F6A','l1' ='#6388B4FF','l2' ='#BD263E','l3' ='#55AD89','l4' = '#FFAE34'),
                      labels = c( expression("  "*lambda*" = 3.5   p = 2.2 "), expression(" "*lambda*" = 2.0   p = 2.2 "), expression(" "*lambda*" = 1.0   p = 2.2 "),
                                  expression("  "*lambda*" = 0.5   p = 2.2 "), expression(" "*lambda*" = 0.3   p = 2.2 ")))

