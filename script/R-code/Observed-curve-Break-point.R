library(tidyverse)
library(strucchange)
library(xlsx)
library(patchwork)
rm(list = ls())

################################################################
# read data simulation with Reff (3,4,5,6,8,10,12)
################################################################
xa1=read.xlsx("四个城市不同R0理论曲线.xlsx",5) %>% 
  select(1:9) %>% rename("time"=1,"t"=2,"case3"=3,"case4"=4,
                         "case5"=5,"case6"=6,"case8"=7,
                         "case10"=8,"case12"=9)  %>% 
  select(-t) %>% 
  mutate(t=1:n(),name="shanghai")

xa2=read.xlsx("四个城市不同R0理论曲线.xlsx",6) %>% 
  select(1:9) %>% rename("time"=1,"t"=2,"case3"=3,"case4"=4,
                         "case5"=5,"case6"=6,"case8"=7,
                         "case10"=8,"case12"=9)  %>% 
  select(-t) %>% 
  mutate(t=1:n(),name="shenzhen")

xa3=read.xlsx("四个城市不同R0理论曲线.xlsx",7) %>% 
  select(1:9) %>% rename("time"=1,"t"=2,"case3"=3,"case4"=4,
                         "case5"=5,"case6"=6,"case8"=7,
                         "case10"=8,"case12"=9)  %>% 
  select(-t) %>% 
  mutate(t=1:n(),name="nanjing")

xa4=read.xlsx("四个城市不同R0理论曲线.xlsx",8) %>% 
  select(1:9) %>% rename("time"=1,"t"=2,"case3"=3,"case4"=4,
                         "case5"=5,"case6"=6,"case8"=7,
                         "case10"=8,"case12"=9)  %>% 
  select(-t) %>% 
  mutate(t=1:n(),name="suzhou")
################################################################
# read data real cases
################################################################
## real data
ob1=read.xlsx("拟合流行曲线.xlsx",1) %>% 
  select(1:2) %>% rename("time"=1,"case"=2) %>% 
  na.omit() %>% 
  mutate(t=1:n(),name="shanghai")

ob2=read.xlsx("拟合流行曲线.xlsx",2) %>% 
  select(1:2) %>% rename("time"=1,"case"=2) %>% 
  na.omit() %>% 
  mutate(t=1:n(),name="shenzhen")

ob3=read.xlsx("拟合流行曲线.xlsx",3) %>% 
  select(1:2) %>% rename("time"=1,"case"=2) %>% 
  na.omit() %>% 
  mutate(t=1:n(),name="nanjing")

ob4=read.xlsx("拟合流行曲线.xlsx",4) %>% 
  select(1:2) %>% rename("time"=1,"case"=2) %>% 
  na.omit() %>% 
  mutate(t=1:n(),name="suzhou")

## get areas under the curve in real data
ara1=function(ti=20,data=xa2){
  ya=data%>% 
    mutate(case=as.vector(scale(case,center = F)))  %>% 
    filter(t<ti) 
  
  x=MESS::auc(ya$t, (ya$case), from=0, rule=2, yleft=0)
  return(x)
}

## get areas under the curve in Theoretical data
ara=function(ti=20,R0=2,data=xa2){
  ya=data %>% select(time,R0) %>% 
    rename("time"=1,"case"=2) %>% 
    na.omit() %>% mutate(t=1:n()) %>% 
    mutate(case=as.vector(scale(case,center = F))) %>% 
    filter(t<ti) 
  
  x=MESS::auc(ya$t, (ya$case), from=0, rule=2, yleft=0)
  return(x)
}


#####################################################################
# 1. shenzhen #######################################################
#####################################################################
### get areas
ar1=c()
for (i in 1: dim(ob2)[1]) {
  xa=ara1(ti=i,data=ob2)
  ar1=c(ar1,xa)
}
# observed
ob2=ob2 %>% 
  mutate(area=ar1)


### get areas in real curve
xa11=tibble()
for (iro in 2:8) {
  xr_0=iro
  r0_name=c(NA,3:6,8,10,12)
  ar1=c()
  for (i in 1:dim(xa2)[1]) {
    xa=ara(ti=i,R0=xr_0,data=xa2)
    ar1=c(ar1,xa)
  }
  # simulation
  xaa1=xa2 %>% select(time,xr_0,t,name) %>% 
    rename("case"=2) %>% 
    mutate(area=ar1,type=paste0("R0=",r0_name[xr_0]))
  print(paste0("R0=",r0_name[xr_0]))
  xa11=xaa1 %>% bind_rows(xa11)
}

## BK points
BKtime=read.csv("Breakpoint-8R0.csv",header = T) %>% 
  as_tibble() %>% select(-1) %>% rename("type"=6) %>% 
  mutate(time=as.Date(time))

BKtime1=BKtime %>% filter(name=="shenzhen") %>% 
  select(t,time,name,type)

## get the proportions in each segmentation
x=left_join(BKtime1,xa11)
xp=tibble()
for (i in unique(x$type)) {
  x1=x %>% filter(type==i)
  xp1=x1 %>% mutate(Proportion=100*area/max(x1$area))
  xp=xp %>% bind_rows(xp1)
}


## Calculating the derivatives
xa=ob2 %>% select(case,t,time,name) 
library(doremi)
der_est <- calculate.gold(signal = xa$case, time = xa$t,embedding = 5)

xb=xa %>% mutate(
  t2=der_est$t,
  d=der_est$dsignal[,2]) 

## plot
ggplot(xb)+
  geom_line(aes(t,case))+
  geom_line(aes(t,d),col="red")

xb %>% arrange(desc(d)) %>% head(1)

## getting the Highest derivatives points
maxarea= ob2 %>% filter(t==42)

x=xp %>% mutate(are_real=Proportion*maxarea$area/100)

## the real curve areas under the proportions by Theoretical curve
xtime=c()
for (i in x$are_real) {
  y=ob2 %>% filter(area<(i+0.01)) %>% 
    arrange(desc(area)) %>% slice(1) 
  print(y$time)
  xtime=c(xtime,as.character(y$time))
}
x$real_time=as.character(xtime)


write.csv(x,"shenzhen-real-BK.csv")


ggplot(ob2)+
  geom_line(aes(t,area))+
  geom_vline(xintercept = 42, linetype="dashed",color="red")


## plot simulations



#####################################################################
# 2.Shanghai#######################################################
#####################################################################
### get areas
ar1=c()
for (i in 1: dim(ob1)[1]) {
  xa=ara1(ti=i,data=ob1)
  ar1=c(ar1,xa)
}
# observed
ob1=ob1 %>% 
  mutate(area=ar1)


### get areas in real curve
xa11=tibble()
for (iro in 2:8) {
  xr_0=iro
  r0_name=c(NA,3:6,8,10,12)
  ar1=c()
  for (i in 1:dim(xa2)[1]) {
    xa=ara(ti=i,R0=xr_0,data=xa1)
    ar1=c(ar1,xa)
  }
  # simulation
  xaa1=xa1 %>% select(time,xr_0,t,name) %>% 
    rename("case"=2) %>% 
    mutate(area=ar1,type=paste0("R0=",r0_name[xr_0]))
  print(paste0("R0=",r0_name[xr_0]))
  xa11=xaa1 %>% bind_rows(xa11)
}

## BK points
BKtime=read.csv("Breakpoint-8R0.csv",header = T) %>% 
  as_tibble() %>% select(-1) %>% rename("type"=6) %>% 
  mutate(time=as.Date(time))

BKtime1=BKtime %>% filter(name=="shanghai") %>% 
  select(t,time,name,type)

## get the proportions in each segmentation
x=left_join(BKtime1,xa11)
xp=tibble()
for (i in unique(x$type)) {
  x1=x %>% filter(type==i)
  xp1=x1 %>% mutate(Proportion=100*area/max(x1$area))
  xp=xp %>% bind_rows(xp1)
}


## Calculating the derivatives
xa=ob1 %>% select(case,t,time,name) 
library(doremi)
der_est <- calculate.gold(signal = xa$case, time = xa$t,embedding = 5)

xb=xa %>% mutate(
  t2=der_est$t,
  d=der_est$dsignal[,2]) 

## plot
ggplot(xb)+
  geom_line(aes(t,case))+
  geom_line(aes(t,d),col="red")

xb %>% arrange(desc(d)) %>% head(1)

## getting the Highest derivatives points
maxarea= ob1 %>% filter(t==33)

x=xp %>% mutate(are_real=Proportion*maxarea$area/100)

## the real curve areas under the proportions by Theoretical curve
xtime=c()
for (i in x$are_real) {
  y=ob2 %>% filter(area<(i+0.01)) %>% 
    arrange(desc(area)) %>% slice(1) 
  print(y$time)
  xtime=c(xtime,as.character(y$time))
}
x$real_time=as.character(xtime)


write.csv(x,"shanghai-real-BK.csv")


ggplot(ob2)+
  geom_line(aes(t,area))+
  geom_vline(xintercept = 42, linetype="dashed",color="red")

## plot simulations

