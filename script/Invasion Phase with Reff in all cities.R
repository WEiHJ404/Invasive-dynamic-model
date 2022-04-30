library(tidyverse)
library(strucchange)
library(xlsx)
library(patchwork)
rm(list = ls())

################################################################
# read data 
################################################################

## shanghai
y3=read.csv("Shanghai-figdf.csv",header = T) %>% select(-1) %>% 
  mutate(type=factor(substr(type,4,6),
                     levels=c("3","4","5","6","8","10","12")))


ggplot(y3)+
  geom_line(aes(t,case,color=type),size=1.5)+
  geom_vline(aes(group=type,xintercept = tat), linetype="dashed",color="red")+
  geom_text(mapping = aes(x = xat-4, y = yat, label = label),size=4.5)+
  facet_grid(type~., 
             scales = "free_x",
             labeller = label_bquote(rows = ~R[italic("eff")]~ "=" ~.(as.character(type))))+
  xlim(0,150)+labs(x="Time",y="Cases (Scaling with normalization)",title = "Nanjing")+
  theme_bw()+
  theme(legend.position  = "none",
        text = element_text(size=20)
  )


ggsave("Shanghai-R0-BK.pdf",width = 12,height = 10,dpi = 300)


## Shenzhen
y3=read.csv("Shenzhen-figdf.csv",header = T) %>% select(-1) %>% 
  mutate(type=factor(substr(type,4,6),
                     levels=c("3","4","5","6","8","10","12")))


ggplot(y3)+
  geom_line(aes(t,case,color=type),size=1.5)+
  geom_vline(aes(group=type,xintercept = tat), linetype="dashed",color="red")+
  geom_text(mapping = aes(x = xat-4, y = yat, label = label),size=4.5)+
  facet_grid(type~., 
             scales = "free_x",
             labeller = label_bquote(rows = ~R[italic("eff")]~ "=" ~.(as.character(type))))+
  xlim(0,150)+labs(x="Time",y="Cases (Scaling with normalization)",title = "Nanjing")+
  theme_bw()+
  theme(legend.position  = "none",
        text = element_text(size=20)
  )


ggsave("Shenzhen-R0-BK.pdf",width = 12,height = 10,dpi = 300)


## nanjing
y3=read.csv("Nanjing-figdf.csv",header = T) %>% select(-1) %>% 
  mutate(type=factor(substr(type,4,6),
                     levels=c("3","4","5","6","8","10","12")))


ggplot(y3)+
  geom_line(aes(t,case,color=type),size=1.5)+
  geom_vline(aes(group=type,xintercept = tat), linetype="dashed",color="red")+
  geom_text(mapping = aes(x = xat-4, y = yat, label = label),size=4.5)+
  facet_grid(type~., 
             scales = "free_x",
             labeller = label_bquote(rows = ~R[italic("eff")]~ "=" ~.(as.character(type))))+
  xlim(0,150)+labs(x="Time",y="Cases (Scaling with normalization)",title = "Nanjing")+
  theme_bw()+
  theme(legend.position  = "none",
        text = element_text(size=20)
  )


ggsave("Nanjing-R0-BK.pdf",width = 12,height = 10,dpi = 300)

## Suzhou
y3=read.csv("Suzhou-figdf.csv",header = T) %>% select(-1) %>% 
  mutate(type=factor(substr(type,4,6),
                     levels=c("3","4","5","6","8","10","12")))


ggplot(y3)+
  geom_line(aes(t,case,color=type),size=1.5)+
  geom_vline(aes(group=type,xintercept = tat), linetype="dashed",color="red")+
  geom_text(mapping = aes(x = xat-4, y = yat, label = label),size=4.5)+
  facet_grid(type~., 
             scales = "free_x",
             labeller = label_bquote(rows = ~R[italic("eff")]~ "=" ~.(as.character(type))))+
  xlim(0,150)+labs(x="Time",y="Cases (Scaling with normalization)",title = "Nanjing")+
  theme_bw()+
  theme(legend.position  = "none",
        text = element_text(size=20)
  )


ggsave("Suzhou-R0-BK.pdf",width = 12,height = 10,dpi = 300)















