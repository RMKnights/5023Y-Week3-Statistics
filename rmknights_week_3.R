### 19/02/2021, Week 3

library(tidyverse)
library(patchwork)
library(broom)

wood_density <- read.csv("Data/wood_density.csv")
### density=independent variable, hardness=dependent variable

wood_density %>% 
  ggplot(aes(x = Density, y = Hardness))+
  geom_point()+
  geom_smooth(method="lm")

density_model <- lm(Hardness~Density, data = wood_density)
density_model
### Hardness=-1160.5+57.51(Density)

### TASK - For lowest wood density value of 24.7, calculate predicted
### predicted hardness manually using the straight line equations 
### and the values of the intercept and the slope

(24.7*57.51)+-1160.5
### 259.997

coef(density_model)[1]+
  coef(density_model)[2]*
  24.7
### 259.9152, this way prevents rounding errors

fitted(density_model)

### residuals=difference between actual value and predicted value
### (predicted value/model-fitted value)

484-259.9152
### 224.0848

427-265.6658
###161.3342

413-409.4325
###3.5675

wood_density_augmented <- wood_density %>% 
  mutate(predictions=fitted(density_model)) %>% 
  mutate(residuals=Hardness-predictions)
### adding predictions and residuals to original data set

p1 <- wood_density_augmented %>% 
  ggplot(aes(x=Density, y=Hardness))+
  geom_line()+
  ggtitle("Full Data")

p2 <- wood_density_augmented %>% 
  ggplot(aes(x=Density, y=predictions))+
  geom_line()+
  ggtitle("Linear trend")

p3 <- wood_density_augmented %>% 
  ggplot(aes(x=Density, y=residuals))+
  geom_hline(yintercept=0, colour="white", size=5)+
  geom_line()+
  ggtitle("Remaining pattern")

p1+p2+p3

broom::glance(density_model)
broom::tidy(density_model, conf.int=TRUE)
broom::augment(density_model, wood_density, interval="confidence") 

plot1 <- broom::augment(density_model, wood_density, interval="confidence") %>% ggplot(aes(x=Density, y=Hardness))+geom_line(aes(x=Density, y=.fitted))+geom_line(aes(x=Density, y=.upper), linetype="dashed")+geom_line(aes(x=Density, y=.lower), linetype="dashed")+geom_point() +ggtitle("Manually fitting linear model \n and confidence intervals")

plot2 <- wood_density %>% ggplot(aes(x=Density, y=Hardness))+geom_smooth(method=lm)+geom_point()+ggtitle("Geom smooth method to plotting \n a linear model")

plot1+plot2

### TASK - Write Up
### Wood density can be used to predict the hardness of timber. For
### every pound per cubic foot increase in the wood density, an average 
### 57.51 increase was seen (F1,34 = 637, P = <0.001, R^2 = 0.94).
