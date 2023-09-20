# script to predicts models: gams and BRTS
# adapted from EcoCast and EcoROMS and BlueWhaleROMS by Heather Welch (UCSC/NOAA)

############# ----------------------------> Paths to change ################
## path to the top directory of this project - doesn't matter what this folder is called, but something in /home/hwelch
path="/Users/heatherwelch/Dropbox/OLE/operationalization"

## path to the top directory of the albacore project - this is the same 'path' pathway as in the albacore tool
## this is because both tools use the same environmental data, and we will only download it once using the albacore code and then point this tool at it
alb_path="/Users/heatherwelch/Dropbox/OLE/operationalization_albacore"

## path to the load libraries r script ("/loadlib-new-albacore.R") will be appended to this path in the function)
source_path="/Users/heatherwelch/Dropbox/OLE/github/OLE_Projects_new/scratch_workspaces/scratch_heather/10_operationalization/V2"
############# ----------------------------> End ################


create_flyer=function(path,source_path,alb_path=alb_path){
  
  ############ 1. Define directories
  
  source(paste0(source_path,"/loadlib-new.R"),chdir=T)
  
  envdir=glue("{alb_path}/daily_prediction_layers")
  outdir <- paste(path,"/model_runs/",sep="")
  logdir=paste(outdir,"logs/",sep="")
  # staticdir=glue("{path}/static_variables/")
  benioffdir=glue("{path}/shiny-www")
  moddir=glue("{path}/models/")
  bboxdir=glue("{path}/bounding_box/")
  logodir=glue("{path}/logo/")

  ############ 2. Define time and dynamic directories
  get_date=Sys.Date()
  # get_date="2019-12-14"
  
  finaldir=glue("{envdir}/{get_date}")
  
  logfile = paste(logdir,"log_",get_date,".txt",sep="") 
  sink(logfile, type=c("output","message"),append = TRUE) #set all output to templog
  
  ############ 4. create output
  print("**************************************************************************************")
  print(paste0("Starting script 4_Create_Flyer,"," Time is ",Sys.time()))
  if(length(list.files(finaldir))==32){

  ############ 8. make final product with metadata
  template=image_read(paste0(logodir,"logo.png"))
  to360 <- function(x) {x %% 360}
  e <- as(extent(to360(-180), to360(-100), 10, 62), 'SpatialPolygons')
  
  # species read in
  print("Reading in a reclassifying species predictions")
  thresholds=read.csv(glue("{logodir}/OLE_model_thresholds.csv"))%>% 
    mutate(species=gsub("_TOPP","",species))%>% 
    mutate(species=gsub("_Dallas","",species))
  speciesList=list.files(benioffdir,pattern = as.character(get_date)) %>%
    grep("core",.,value=T,invert = T) %>% 
    grep(".grd",.,value=T) %>% 
    gsub(as.character(get_date),"",.) %>% 
    gsub("_.grd","",.)
  
  empty=list()
  for(i in 1:length(speciesList)){
    ras=glue("{benioffdir}/{speciesList[i]}_{as.character(get_date)}.grd") %>% raster()
    sp_thresh=thresholds %>% filter(species==speciesList[i]) %>% pull(X50_threshold)
    ras[values(ras)>=sp_thresh]=1
    ras[values(ras)<sp_thresh]=0
    empty[[length(empty)+1]]=ras
  }
  
  master_ras=stack(empty) %>% sum(.,na.rm=T) %>% crop(.,e)
  
  print("Making map")
  
  ## getting cclme
  
  # cclme=st_read(glue("{logodir}lme/lme.shp")) %>% as_Spatial()
  # cclme360=recenter(cclme) %>% st_as_sf()
  # cclme360sp=recenter(cclme)
  # 
  
  sf::sf_use_s2(FALSE)
  data=maps::map("world2",fill=T)
  IDs <- sapply(strsplit(data$names, ":"), function(x) x[1])
  wrld_simpl <- map2SpatialPolygons(data, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
  wrld=SpatialPolygons(wrld_simpl@polygons,proj4string=wrld_simpl@proj4string) %>% 
    gBuffer(., byid=TRUE, width=0)
  wrld2=st_as_sf(wrld) %>% st_crop( xmin=180, xmax=260, ymax=62, ymin=10) %>% as_Spatial()
  
  plot_date=as.character(get_date, format="%B %d %Y")
  
  df_maphigh=rasterToPoints(master_ras)%>% as.data.frame()
  colnames(df_maphigh)=c("rows","cols","value")
  
  df <- data.frame(xmin=180, xmax=260, ymin=10, ymax=62)
  breaks=pretty(df_maphigh$value) %>% round(.,0)
  
  sp_plot=ggplot()+
    geom_tile(data=df_maphigh,aes(x = rows, y = cols, fill=value))+
    # geom_polygon(data=cclme360,aes(x = long, y = lat),fill=NA,color="red")+
    scale_fill_gradientn("# of species",colours = pals::parula(100),na.value="black",breaks=breaks,labels=breaks)+
    theme_classic()+xlab(NULL)+ylab(NULL)+
    geom_rect(data=df,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color = "black",fill=NA)+
    geom_polygon(data=fortify(wrld2,plot=FALSE,fill=TRUE),aes(x=long, y = lat, group=group),color="black",fill="grey")+
    coord_map("conic", lat0 = 30,xlim = c(180, 260), ylim = c(10,62))+
    # coord_sf(xlim = c(180,260),
    #          ylim = c(10,62))+
    ggtitle(glue("Top predator core habitat {plot_date}"))+
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank(),
          plot.title = element_text(hjust = 0.5))+
    theme(legend.position = "bottom")
  
  png(glue("{benioffdir}/core_{as.character(get_date)}.png"),width=12,height=12,units='cm',res=400,type = "cairo")
  par(ps=10)
  par(mar=c(4,4,1,1))
  par(cex=1)
  print({sp_plot})
  dev.off()
  
  # cc_clip=mask(master_ras,cclme360)
  # df_maphigh=rasterToPoints(cc_clip)%>% as.data.frame()
  # colnames(df_maphigh)=c("rows","cols","value")
  
  # cc_plot=ggplot()+
  #   geom_tile(data=df_maphigh,aes(x = rows, y = cols, fill=value))+
  #   geom_sf(data=cclme360,fill=NA,color="red")+
  #   scale_fill_gradientn("# of species",colours = pals::parula(100),na.value="black",breaks=breaks,labels=breaks)+
  #   theme_classic()+xlab(NULL)+ylab(NULL)+
  #   # geom_polygon(data=fortify(wrld2,plot=FALSE,fill=TRUE),aes(x=long, y = lat, group=group),color="black",fill="grey")+
  #   # coord_map("conic", lat0 = 30,xlim = c(180, 260), ylim = c(10,62))+
  #   coord_sf(xlim = c(225,250),
  #            ylim = c(22,47))+
  #   ggtitle(glue("California Current LME"))+
  #   theme(axis.line=element_blank(),axis.text.x=element_blank(),
  #         axis.text.y=element_blank(),axis.ticks=element_blank(),
  #         axis.title.x=element_blank(),
  #         axis.title.y=element_blank(),
  #         panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
  #         panel.grid.minor=element_blank(),plot.background=element_blank(),
  #         plot.title = element_text(hjust = 0.5))+
  #   theme(legend.position = "none")
  # 
  # png(glue("{benioffdir}/cc_{as.character(get_date)}.png"),
  #     width=9,height=12,units='cm',res=400,type = "cairo",
  #     bg="transparent")
  # par(ps=10)
  # par(mar=c(1,1,1,1))
  # par(cex=1)
  # print({cc_plot})
  # dev.off()
  
  ## adult
  benioff=image_read(glue("{benioffdir}/core_{get_date}.png"))
  globe=image_read(glue("{logodir}/globe.png"))
  # cc=image_read(glue("{benioffdir}/cc_{get_date}.png"))

  template2=image_scale(template, "600")
  benioff2=image_scale(benioff, "520")
  globe2=image_scale(globe, "135")
  # cc2=image_scale(cc, "190")
  
  # a=image_composite(template2,cc2,offset = "+390+160")
  # b=image_composite(a,globe2,offset = "+8+160")
  # c=image_composite(b,benioff2,offset = "+37+122")
  
  a=image_composite(template2,benioff2,offset = "+37+122")
  b=image_composite(a,globe2,offset = "+8+170")
  # c=image_composite(b,cc2,offset = "+415+160")
  # c
  
  d=image_annotate(b,
                   paste0("Image created ",Sys.Date()," by Heather Welch. Next projected image date: ",Sys.Date()+1),
                   size=9,gravity = "southeast",location="+100+270",font = "courier")
  
  image_write(d,glue("{benioffdir}/latest/core_flyer_latest.png"))
  image_write(d,glue("{benioffdir}/core_flyer_{get_date}.png"))

  
  print("**************************************************************************************")
  sink(NULL)
  }

}

create_flyer(path=path,source_path=source_path,alb_path=alb_path)


