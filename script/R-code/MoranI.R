library(spdep)
library(tidyverse)
library(sf)
rm(list = ls())
select=dplyr::select
filter=dplyr::filter

source("hexmap.R")


######################################################
## getting data in Suzhou
Suzhou=read_sf("https://geo.datav.aliyun.com/areas_v2/bound/320500_full.json")
dfsz=xlsx::read.xlsx("/Users/Anderson/Desktop/COVID-Om/数据/数据收集—suzhou (V0404).xlsx",7)
dfsz %>% 
  filter(lng>119) %>% 
  filter(lat<34 & lat>30) %>% 
  ggplot() +
  geom_point(aes(lng,lat),color="#de4307")+
  geom_sf(data=Suzhou,fill=NA) +
  labs(title = "Number of COVID-19 (Omicron) patients in Suzhou",x="",y="")+
  theme_minimal() 

ggsave("Suzhoumap.pdf",height = 8,width = 8,dpi=500)


##### 2/4-1/31
ncount=dfsz %>% mutate(covid=1) %>% 
  filter(time<as.Date("2022-03-2")) %>% 
  select(lng,lat) %>% 
### Moran I statistic standard
xa=hexmap(map=Suzhou)
get_moran(df=ncount,map=xa,mapx = Suzhou)
ggsave("Suzhoumap1.pdf",height = 8,width = 8,dpi=500)


######################################################
## getting data in Shenzhen
dfsz=xlsx::read.xlsx("/Users/Anderson/Desktop/COVID-Om/数据/data-深圳-20220403.xlsx",6)
Shenzhen=read_sf("https://geo.datav.aliyun.com/areas_v2/bound/440300_full.json")
st_is_valid(Shenzhen)

dfsz %>% 
  ggplot() +
  geom_point(aes(lng,lat),color="#de4307")+
  geom_sf(data=Shenzhen,fill=NA) +
  labs(title = "Number of COVID-19 (Omicron) patients in Shenzhen",x="",y="")+
  theme_minimal() 

ggsave("Shenzhen.pdf",height = 8,width = 8,dpi=500)

##### 1/31-2/6 colonization
ncount=dfsz %>% mutate(covid=1) %>% 
  filter(time<as.Date("2022-02-7")&time>as.Date("2022-01-31")) %>% 
  select(lng,lat)
### Moran I statistic standard
xa=hexmap(map=Shenzhen)
get_moran(df=ncount,map=xa,mapx = Shenzhen)
ggsave("Shenzhenmap1.pdf",height = 8,width = 8,dpi=500)



##### 2/6-2/18 Establisment
### bind covid case in each section
ncount=dfsz %>% mutate(covid=1) %>% 
  filter(time<as.Date("2022-02-19")& time>as.Date("2022-02-6")) %>% 
  select(lng,lat)
### Moran I statistic standard
get_moran(df=ncount,map=xa,mapx = Shenzhen)
ggsave("Shenzhenmap2.pdf",height = 8,width = 8,dpi=500)



##### 2/18-3/3 Spread
### bind covid case in each section
ncount=dfsz %>% mutate(covid=1) %>% 
  filter(time<as.Date("2022-03-4")& time>as.Date("2022-02-18")) %>% 
  select(lng,lat)
### Moran I statistic standard
get_moran(df=ncount,map=xa,mapx = Shenzhen)
ggsave("Shenzhenmap3.pdf",height = 8,width = 8,dpi=500)



##### 3/10 outbreak
### bind covid case in each section
ncount=dfsz %>% mutate(covid=1) %>% 
  filter(time>as.Date("2022-03-3")) %>% 
  select(lng,lat)
### Moran I statistic standard
get_moran(df=ncount,map=xa,mapx = Shenzhen)
ggsave("Shenzhenmap4.pdf",height = 8,width = 8,dpi=500)



######################################################
## Shanghai
dfsh=read.csv("/Users/Anderson/Desktop/COVID-Om/数据/上海病例地理位置信息.csv",header = T)
Shanghai=read_sf("https://geo.datav.aliyun.com/areas_v3/bound/310000_full.json") 
st_is_valid(Shanghai)

dfsh %>% 
  mutate(Time=value) %>% 
  ggplot() +
  geom_point(aes(lng,lat),color="#de4307")+
  geom_sf(data=Shanghai,fill=NA) +
  labs(title = "Number of COVID-19 (Omicron) patients in Shanghai",x="",y="")+
  theme_minimal() 

ggsave("Shanghaimap.pdf",height = 8,width = 8,dpi=500)


##### 3/1-3/7  colonization
### bind covid case in each section
ncount=dfsh %>% mutate(covid=1) %>% 
  mutate(time=as.Date(value)) %>% 
  filter(time<as.Date("2022-03-08")& time >as.Date("2022-03-01")) %>% 
  select(lng,lat)

### Moran I statistic standard
get_moran(df=ncount,map=xa,mapx = Shanghai) #get Moran's I

get_moransh(df=ncount,map=xa,mapx = Shanghai)# get map
ggsave("shanghaimap1.pdf",height = 8,width = 8,dpi=500)




##### 3/7-3/24 Establishment
### bind covid case in each section
ncount=dfsh %>% mutate(covid=1) %>% 
  mutate(time=as.Date(value)) %>% 
  filter(time<as.Date("2022-03-24")& time >as.Date("2022-03-07")) %>% 
  select(lng,lat)

### Moran I statistic standard
get_moran(df=ncount,map=xa,mapx = Shanghai)

xa=hexmap(map=Shanghai,full = F)
get_moransh(df=ncount,map=xa,mapx = Shanghai)
ggsave("shanghaimap2.pdf",height = 8,width = 8,dpi=500)

##### 3/24-4/2 Spread
### bind covid case in each section
ncount=dfsh %>% mutate(covid=1) %>% 
  mutate(time=as.Date(value)) %>% 
  filter(time<as.Date("2022-04-2")& time >as.Date("2022-03-23")) %>% 
  select(lng,lat)

### Moran I statistic standard
get_moran(df=ncount,map=xa,mapx = Shanghai)

xa=hexmap(map=Shanghai,full = F)
get_moransh(df=ncount,map=xa,mapx = Shanghai)
ggsave("shanghaimap3.pdf",height = 8,width = 8,dpi=500)

##### 4/2 Outbreak
### bind covid case in each section
ncount=dfsh %>% mutate(covid=1) %>% 
  mutate(time=as.Date(value)) %>% 
  filter( time>as.Date("2022-04-1")) %>% 
  select(lng,lat) 

### Moran I statistic standard
get_moran(df=ncount,map=xa,mapx = Shanghai)

xa=hexmap(map=Shanghai,full = F)
get_moransh(df=ncount,map=xa,mapx = Shanghai)
ggsave("shanghaimap4.pdf",height = 8,width = 8,dpi=500)




######################################################
######################################################
## Nanjing
Nanjing=read_sf("https://geo.datav.aliyun.com/areas_v3/bound/320100_full.json")
dfnj=xlsx::read.xlsx("/Users/Anderson/Desktop/COVID-Om/数据/数据收集-nanjing (V0404).xlsx",7)
st_is_valid(Nanjing)

Nanjing1=Nanjing %>% slice(-c(7,11))
st_is_valid(Nanjing1)
##### 3/10-3/16
ncount=dfnj %>% mutate(covid=1) %>% 
  filter(time<as.Date("2022-03-17")) %>% 
  select(lng,lat)
### Moran I statistic standard
xa=hexmap(map=Nanjing1,full = F)
get_moran(df=ncount,map=xa,mapx = Nanjing1)

ggsave("Nanjingmap1.pdf",height = 8,width = 8,dpi=500)





