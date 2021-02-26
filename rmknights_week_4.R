### 26/02/2020 Week 4 - Comparing groups and calculating estimates

library(tidyverse)
library(emmeans)

darwin <- read.csv("Data/darwin.csv")

darwin <- darwin %>% 
  pivot_longer(cols = c("Self":"Cross"),
               names_to = "type",
               values_to = "height") %>% 
  mutate(type=factor(type,
                     levels = c("Self",
                                "Cross"))) %>% 
  mutate(pair=factor(pair))

model <- lm(formula = height ~ type + pair, data = darwin)
summary(model)
summary(aov(model))

estimates <- emmeans(model, specs="type")
estimates %>% 
  as_tibble %>% 
  ggplot(aes(x=type, 
             y=emmean, 
             colour=type))+
  geom_pointrange(aes(ymin=lower.CL, 
                      ymax=upper.CL))+
  geom_pointrange(aes(ymin=emmean-SE, 
                      ymax=emmean+SE), 
                  size=1.2)
### thick lines = standard error(66% confidence levels), thin lines =
### 95% confidence levels

tidymodel1 <- broom::tidy(model) %>% 
  mutate(lwr=((estimate-(std.error*2))),
         upr=(estimate+(std.error*2)))
tidymodel1 %>% 
  ggplot(aes(x=estimate, 
             y=term))+
  geom_pointrange(aes(xmin=lwr, 
                      xmax=upr))+
  geom_vline(xintercept=0, 
             linetype="dashed")
### anything which crosses the 0 is not significantly different at 
### P<0.05 from the mean calculated for the intercept

tidymodel2 <- broom::tidy(model, conf.int=T)
tidymodel2[2,] ### will only pull out row 2

tidymodel2 %>% 
  ggplot(aes(x=estimate, 
             y=term))+
  geom_pointrange(aes(xmin=conf.low, 
                      xmax=conf.high))+
  geom_vline(xintercept=0, 
             linetype="dashed")

t.crit <- qt(0.975, df=14)

upr <- 2.62+t.crit*1.22
lwr <- 2.62-t.crit*1.22

### A paired t-test showed that the cross-pollinated maize were 
### significantly taller than the self-pollinated plants by an average 
### of 2.6 inches (95% CI: 0.004-5.23).