library(tidyverse)

mean = 500
sd = 1
x0 = (mean-4*sd)
x1 = (mean+4*sd)
y0 = (mean-sd)
y1 = (mean+sd)

funcShaded <- function(x) {
  y <- dnorm(x, mean = mean, sd = sd)
  #y[x < y0 | x > y1] <- NA
  y[x < y0] <- NA
  #y[x < y0 | x > y1] <- NA
  return(y)
}

ggplot(data.frame(x = c(x0, x1)), aes(x = x)) +
  stat_function(fun = dnorm, args = list(mean, sd), size = 1, alpha = 0.8)+
  stat_function(fun=funcShaded, geom="area", fill="blue", alpha=0.2)+
  geom_hline(yintercept = 0, size = 1.1, alpha = 0.6)+
  labs(title = "Probabilidade",
       x = "Média",
       #y = "",
       caption = "Aquarela Advanced Analytics")+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(face = "bold", size = 12),
        plot.caption = element_text(size = 15, face = "bold")
        #panel.background = element_blank()
        ) 
  

# Menor que
pnorm(q = -1, mean = 0, sd = 1)

# Maior que
1-pnorm(q = -1, mean = 0, sd = 1)

pnorm(q = 1, mean = 0, sd = 1)

# P entre
(pnorm(q = 1, mean = 0, sd = 1) - pnorm(q = -1, mean = 0, sd = 1))







f1 <- function(x){x^3+x^2-40}
f2 <- function(x){(3*x^2)+(+2*x)}
f3 <- function(x){6*x+2}

ggplot(data.frame(x = c(-1, 2)), aes((x=x)))+
#  stat_function(fun = f1)+
  stat_function(fun = f2)+
  stat_function(fun = f3)


