## create indicators for Dale, benioff

############# ----------------------------> Paths to change ################
## path to the top directory of this project - doesn't matter what this folder is called, but something in /home/hwelch
path="/Users/heatherwelch/Dropbox/benioff_operationalization/operationalization_NOAA"

## path to the load libraries r script ("/loadlib-new.R") will be appended to this path in the function)
source_path="/Users/heatherwelch/Dropbox/benioff_operationalization/BlueWhaleROMS/Operationalizing/V2_NOAA"

############# ----------------------------> End ################

create_indicators=function(path,source_path){
  source(paste0(source_path,"/loadlib-new.R"),chdir=T)
  
  envdir=glue("{path}/daily_prediction_layers")
  outdir <- glue("{path}/BenioffRuns")
  staticdir=glue("{path}/static_variables")
  temp=glue("{path}/raw_roms_data")
  moddir=glue("{path}/models")
  intermediatedir=glue("{path}/intermediate")
  
  flyersdir=glue("{outdir}/flyers")
  rastersdir=glue("{outdir}/rasters")
  mapssdir=glue("{outdir}/maps")
  latestdir=glue("{outdir}/latest")
  latestweekdir=glue("{outdir}/latest_week")
  latestweeksmootheddir=glue("{outdir}/latest_week_smoothed")
  indicatorsdir=glue("{outdir}/indicators")
  
  ## read in spatial data ####
  sf=st_read(glue("{staticdir}/vsr_zone_sf/sf_vsr_zone.shp")) %>% 
    st_combine()
  sfp=as_Spatial(sf)
  
  WP=st_read(glue("{staticdir}/shp/WesternPolygon.shp"))
  WP <- sf:::st_zm(WP$geom)
  WP=as(WP, "Spatial")
  outsidelaneshp <-spTransform(WP, CRS("+proj=longlat +datum=WGS84")) #convert from UTM to LatLon
  
  TSS=st_read(glue("{staticdir}/shp/TSSpolygon.shp"))
  TSS <- sf:::st_zm(TSS$geom)
  TSS=as(TSS, "Spatial")
  insidelaneshp <-spTransform(TSS, CRS("+proj=longlat +datum=WGS84")) #convert from UTM to LatLon
  a=st_as_sf(outsidelaneshp)
  b=st_as_sf(insidelaneshp)
  combined=st_union(a,b)
  
  scb_coords=matrix(c(-121.0299, 33.29988,  ## define SST box
                      -121.0299,34.57384,
                      -117.4803,34.57384,
                      -117.4803, 33.29988,
                      -121.0299, 33.29988),
                    ncol=2,byrow = T)
  
  p=Polygon(scb_coords)
  ps=Polygons(list(p),1)
  sps = SpatialPolygons(list(ps))
  
  ## read in indicators ####
  indicator=read.csv(glue("{indicatorsdir}/lane_indicator.csv")) %>% dplyr::select(-c(X)) %>% 
    mutate(Date=ymd(Date))
  indicator_benioff=read.csv(glue("{indicatorsdir}/VSR_indicator.csv")) %>% dplyr::select(-c(X)) %>% 
    mutate(Date=ymd(Date))
  indicator_sf=read.csv(glue("{indicatorsdir}/VSR_indicator_sf_new.csv")) %>% dplyr::select(-c(X)) %>% 
    mutate(Date=ymd(Date))
  
  ## dates and rasters ####
  # get_date=as.character(Sys.Date()-1)
  # get_date_1=as.character(Sys.Date()-2)
  # get_date_2=as.character(Sys.Date()-3)
  # get_date_3=as.character(Sys.Date()-4)
  # get_date_4=as.character(Sys.Date()-5)
  # get_date_5=as.character(Sys.Date()-6)
  # get_date_6=as.character(Sys.Date()-7)
  # get_date_7=as.character(Sys.Date()-8)
  # get_date_8=as.character(Sys.Date()-9)
  
  toMatch=seq(Sys.Date()-30,Sys.Date(),by=1) #%>% as.character()
  # test=seq(as.Date("2013-05-16"),as.Date("2013-07-16"),by=1)
  
  # toMatch=list(get_date,get_date_1,get_date_2,get_date_3,get_date_4,get_date_5,get_date_6,get_date_7,get_date_8) %>% unlist()
  
  files=list.files(rastersdir,pattern = paste(toMatch,collapse="|"),full.names = T) %>% grep(".grd",.,value = T) 
  if(length(files)>0){ ## if there is new data to add
  raster=stack(files)
  dates=files %>% gsub(rastersdir,"",.) %>% gsub("/blwh_ensemble_","",.) %>% 
    gsub(".grd","",.)
  
  ## lane indicator #####
  ind=indicator %>% filter(!(Date %in% toMatch)) %>% 
    arrange(Date)
  
  inside=raster::extract(raster,insidelaneshp,fun=base::mean,df=T) %>% 
    dplyr::select(-ID) %>% 
    gather(Date,value)%>% 
    mutate(lane="inside") %>% 
    mutate(Date=dates) 
  
  outside=raster::extract(raster,outsidelaneshp,fun=base::mean,df=T) %>% 
    dplyr::select(-ID) %>% 
    gather(Date,value)%>% 
    mutate(lane="outside") %>% 
    mutate(Date=dates) 
  
  both=raster::extract(raster,combined,fun=base::mean,df=T) %>% 
    dplyr::select(-ID) %>% 
    gather(Date,value)%>% 
    mutate(lane="both") %>% 
    mutate(Date=dates) 
  
  new_lane=do.call("rbind",list(inside,outside,both)) %>% 
    mutate(Date=as.Date(Date)) %>% 
    arrange(Date)
  
  final_lane=rbind(ind,new_lane) %>% arrange(Date)
  write.csv(final_lane,glue("{indicatorsdir}/lane_indicator.csv"))
  
  finall=final_lane[complete.cases(final_lane),]
  
  plot=ggplot(finall[(nrow(finall)-550):nrow(finall),],aes(x=Date,y=value,group=lane))+geom_line(aes(color=lane))+
    scale_x_date(labels = date_format("%m-%Y"))+theme_classic()+ylab("Average habitat suitabiltiy")
  
  png(glue("{indicatorsdir}/lane_indicator.png"), width=7, height=4, units="in", res=400)
  print({plot})
  dev.off()
 
  ## SF indicator ####
  ind_SF=indicator_sf %>% filter(!(Date %in% toMatch)) %>% 
    arrange(Date)
  
  new_sf=raster::extract(raster,sfp,fun=base::mean,df=T,na.rm=T) %>% 
    dplyr::select(-ID) %>% 
    gather(Date,value)%>% 
    mutate(Date=dates) 
  
  final_sf=rbind(ind_SF,new_sf) %>% arrange(Date)
  write.csv(final_sf,glue("{indicatorsdir}/VSR_indicator_sf_new.csv"))
  
  finall=final_sf[complete.cases(final_sf),]
  
  plot=ggplot(finall[(nrow(finall)-730):nrow(finall),],aes(x=Date,y=value))+geom_line(color="black")+
    scale_x_date(labels = date_format("%m-%Y"))+theme_classic()+ylab("Average habitat suitabiltiy")
  
  png(glue("{indicatorsdir}/VSR_indicator_sf_new.png"), width=7, height=4, units="in", res=400)
  print({plot})
  dev.off()
  
  ## Benioff indicator ####
  ind_benioff=indicator_benioff %>% filter(!(Date %in% toMatch)) %>% 
    arrange(Date)
  
  new_benioff=raster::extract(raster,sps,fun=base::mean,df=T,na.rm=T) %>% 
    dplyr::select(-ID) %>% 
    gather(Date,value)%>% 
    mutate(Date=dates) 
  
  final_benioff=rbind(ind_benioff,new_benioff) %>% arrange(Date)
  write.csv(final_benioff,glue("{indicatorsdir}/VSR_indicator.csv"))
  
  finall=final_benioff[complete.cases(final_sf),]
  
  plot=ggplot(finall[(nrow(finall)-730):nrow(finall),],aes(x=Date,y=value))+geom_line(color="black")+
    scale_x_date(labels = date_format("%m-%Y"))+theme_classic()+ylab("Average habitat suitabiltiy")
  
  png(glue("{indicatorsdir}/VSR_indicator.png"), width=7, height=4, units="in", res=400)
  print({plot})
  dev.off()
  
  }
  
}

create_indicators(path=path,source_path=source_path)