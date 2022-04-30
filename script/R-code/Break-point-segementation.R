library(tidyverse)
library(strucchange)
library(xlsx)
rm(list = ls())

# read data
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

bkfun=function(R2=2,data=xa1,label="R0=8"){
  xa=data %>% select(R2,t,time,name) %>% 
    rename("case"=1) %>% na.omit()
  ## calculate first derivative of time series
  library(doremi)
  der_est <- calculate.gold(signal = xa$case, time = xa$t,embedding = 5)
  
  
  xb=xa %>% mutate(
    t2=der_est$t,
    d=der_est$dsignal[,2]) 
  tmax=xb %>% arrange(desc(d)) %>% slice(1) 
  ## plot
  ggplot(xb)+
    geom_line(aes(t,case))+
    geom_line(aes(t,d),col="red")
  ## get max time
  Ti=xb %>% filter(d==max(na.omit(xb$d))) %>% select(t) %>% pull
  
  ya=as.ts(xa$case[1:Ti],start = 1, end = numeric(), frequency = 1)
  
  # store the breakdates
  bp_ts <- breakpoints(ya~c(1:Ti),h = 3)
  
  # this will give you the break dates and their confidence intervals
  bk = summary(bp_ts) 
  
  bkt=as.numeric(na.omit(bk$breakdates[3,]))
  bkt=c(bkt, tmax$t)
  # store the breakdates
  output=xb %>% filter(t %in% bkt) %>% 
    mutate(R0=label)
  return(output)
}

## shanghai
x1=bkfun(R2=2,data = xa1,label="R0=3")
x2=bkfun(R2=3,data = xa1,label="R0=4")
x3=bkfun(R2=4,data = xa1,label="R0=5")
x4=bkfun(R2=5,data = xa1,label="R0=6")
x5=bkfun(R2=6,data = xa1,label="R0=8")
x6=bkfun(R2=7,data = xa1,label="R0=10")
x7=bkfun(R2=8,data = xa1,label="R0=12")

xx1=rbind(x1,x2,x3,x4,x5,x6,x7)

## shenzheng
x1=bkfun(R2=2,data = xa2,label="R0=3")
x2=bkfun(R2=3,data = xa2,label="R0=4")
x3=bkfun(R2=4,data = xa2,label="R0=5")
x4=bkfun(R2=5,data = xa2,label="R0=6")
x5=bkfun(R2=6,data = xa2,label="R0=8")
x6=bkfun(R2=7,data = xa2,label="R0=10")
x7=bkfun(R2=8,data = xa2,label="R0=12")

xx2=rbind(x1,x2,x3,x4,x5,x6,x7)
## nanjing
x1=bkfun(R2=2,data = xa3,label="R0=3")
x2=bkfun(R2=3,data = xa3,label="R0=4")
x3=bkfun(R2=4,data = xa3,label="R0=5")
x4=bkfun(R2=5,data = xa3,label="R0=6")
x5=bkfun(R2=6,data = xa3,label="R0=8")
x6=bkfun(R2=7,data = xa3,label="R0=10")
x7=bkfun(R2=8,data = xa3,label="R0=12")

xx3=rbind(x1,x2,x3,x4,x5,x6,x7)
## suzhou
x1=bkfun(R2=2,data = xa4,label="R0=3")
x2=bkfun(R2=3,data = xa4,label="R0=4")
x3=bkfun(R2=4,data = xa4,label="R0=5")
x4=bkfun(R2=5,data = xa4,label="R0=6")
x5=bkfun(R2=6,data = xa4,label="R0=8")
x6=bkfun(R2=7,data = xa4,label="R0=10")
x7=bkfun(R2=8,data = xa4,label="R0=12")

xx4=rbind(x1,x2,x3,x4,x5,x6,x7)

## save all
x=rbind(xx1,xx2,xx3,xx4)
write.csv(x,"Breakpoint-8R0.csv")




x %>% filter(R0=="R0=10")







