library(sf)
library(raster)
library(dggridR)
library(tidyverse)
library(spdep)

hexmap=function(map=Suzhou,full=F){
  
  ## Set the bounding of a city feature
  bb <- st_bbox(map) %>% 
    st_as_sfc() %>% 
    st_sf()
  # random points
  pts <- st_sample(bb, 5000) %>% 
    st_sf(as.data.frame(st_coordinates(.)), geometry = .) %>% 
    rename(lat = Y, lon = X)
  
  # contruct a hexagonal grid with ~ 5 km between cells
  dggs <- dgconstruct(spacing = 5)
  # for each point, get the grid cell
  pts$cell <- dgGEO_to_SEQNUM(dggs, pts$lon, pts$lat)$seqnum
  
  # sample one checklist per grid cell
  # generate polygons for the grid cells
  hexagons <- dgcellstogrid(dggs, unique(pts$cell)) %>% 
    st_as_sf()
 
  #### 根据shp文件进行抠图
  library(rgdal)
  maps=map %>% 
    st_as_sf(crs ="+proj=longlat +ellps=WGS84")
  
  hex_cen=st_centroid(hexagons) %>% 
    st_coordinates() %>% as_tibble() %>% 
    mutate(id=1:n())
  
  spg =as.data.frame(hex_cen)
  # 1)point change to SpatialPixelsDataFrame
  coordinates(spg) = ~ X + Y
  proj4string(spg) =  CRS("+proj=longlat +ellps=WGS84")
  # 2)SHP change to SpatialPixelsDataFrame
  Yun_shp = as(map, 'Spatial')
  proj4string(Yun_shp) =  CRS("+proj=longlat +ellps=WGS84")
  library(spatialEco)
  # 3) intersect points in polygon
  df_overlap_sp = point.in.poly(spg, Yun_shp)
  
  # convert to data frame, keeping your data
  df_overlap = as.data.frame(df_overlap_sp) %>% na.omit() %>% 
    select(coords.x1,coords.x2,id) %>% 
    set_names("X","Y","id") %>% as_tibble()

  hexagons_map=cbind(hexagons,hex_cen) %>% 
    filter(id %in% df_overlap$id)
  if (full==T){
    return(hexagons)
  } else {return(hexagons_map)}
}


get_moran=function(df=dfsz,map=xa,mapx=Suzhou){
  
  pts1 =  df %>% # Convert data frame to sf object
    select(lng,lat) %>% 
    mutate(lons=lng,lats=lat) %>% 
    st_as_sf(coords = c("lng","lat"),
             crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") 
  
  # contruct a hexagonal grid with ~ 5 km between cells
  dggs <- dgconstruct(spacing = 5)
  # for each point, get the grid cell
  pts1$seqnum <- dgGEO_to_SEQNUM(dggs, pts1$lons, pts1$lats)$seqnum
  
  ## count number of case in each grid
  da=as.data.frame(table(pts1$seqnum)) %>% 
    set_names("seqnum","val") %>% 
    mutate(seqnum=as.numeric(as.character(seqnum)))
  
  ## map
  df=left_join(xa,da) %>% 
    mutate(val = replace_na(val, 0))
  
  OA.Census=df %>% filter(!seqnum==13054153)
  neighbours2 <- poly2nb(OA.Census, queen = FALSE)
  listw <- nb2listw(neighbours2)
  print(listw)
  print(rep("**",30))
  globalMoran <- moran.test(OA.Census$val, listw)
  print(globalMoran)
  
  p=ggplot() +
    geom_sf(data = mapx,fill=NA)+
    geom_sf(data=xa,fill=0.2,color="#AEC5EB") +
    geom_sf(data = pts1,color="red",size=3) +
    #geom_sf(data = pts_ss, col = "red") +
    theme_bw()
  return(p)
  
}

get_moransh=function(df=dfsz,map=xa,mapx=Suzhou){
  xa=hexmap(map=Shanghai,full = F)
  pts1 =  df %>% # Convert data frame to sf object
    select(lng,lat) %>% 
    mutate(lons=lng,lats=lat) %>% 
    st_as_sf(coords = c("lng","lat"),
             crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") 
  
  
  p=ggplot() +
    geom_sf(data = mapx,fill=NA)+
    geom_sf(data=xa,fill=0.2,color="#AEC5EB") +
    geom_sf(data = pts1,color="red",size=3) +
    #geom_sf(data = pts_ss, col = "red") +
    theme_bw()
  return(p)
  
}



