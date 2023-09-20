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

predict_models=function(path,source_path,alb_path){
  
  ############ 1. Define directories
  
  # source(paste0(source_path,"load_libraries.R"),chdir=T)
  source(paste0(source_path,"/loadlib-new.R"),chdir=T)
  
  envdir=glue("{alb_path}/daily_prediction_layers")
  outdir <- paste(path,"/model_runs/",sep="")
  logdir=paste(outdir,"logs/",sep="")
  staticdir=glue("{path}/static_variables/")
  benioffdir=glue("{path}/shiny-www")
  moddir=glue("{path}/models/")
  bboxdir=glue("{path}/bounding_box/")
  
  ############ 2. Define time and dynamic directories
  get_date=Sys.Date()
  # get_date="2019-12-14"
  
  finaldir=glue("{envdir}/{get_date}")
  logfile = paste(logdir,"log_",get_date,".txt",sep="") 
  sink(logfile, type=c("output","message"),append = TRUE) #set all output to templog

  ############ 3. define functions
  predict_models=function(sp_name){
    print(glue("Stating model prediction for {sp_name}"))
    mod=readRDS(glue("{moddir}species_bernoulli_03_22_21_step_lr0.01_tc3_bf0.6_tol1e-05_bernoulli_{sp_name}.rds"))
    # bbox=readOGR(glue("{bboxdir}{sp_name}.shp"))
    bbox=st_read(glue("{bboxdir}{sp_name}.shp"))
    write_out_name=gsub("_TOPP","",sp_name) %>% 
      gsub("_Dallas","",.)
    
    co_stack2=raster::mask(co_stack,bbox)
    pred=predict(co_stack2,mod,n.trees=mod$gbm.call$best.trees,type="response",na.rm=F)
    pred2=raster::mask(pred,bbox)
    writeRaster(pred2,glue("{benioffdir}/{write_out_name}_{get_date}.grd"),overwrite=T)
  }
  
  make_png=function(sp_name,save_name){
    write_out_name=gsub("_TOPP","",sp_name) %>% 
      gsub("_Dallas","",.)
    
    print(glue("Making png prediction for {sp_name}"))
    pred=raster(glue("{benioffdir}/{write_out_name}_{get_date}.grd"))
    # bbox=readOGR(glue("{bboxdir}{sp_name}.shp"))
    bbox=st_read(glue("{bboxdir}{sp_name}.shp"))
   
    
    df_maphigh=rasterToPoints(pred)%>% as.data.frame()
    colnames(df_maphigh)=c("rows","cols","value")
    
    plot_sp=ggplot()+
      geom_tile(data=df_maphigh,aes(x = rows, y = cols, fill=value))+
      scale_fill_gradientn("Habitat suitability",colours = pals::parula(100),na.value="black")+
      geom_polygon(data = fortify(maps::map("world2",plot=FALSE,fill=TRUE)), aes(x=long, y = lat, group=group),color="black",fill="grey")+
      # geom_polygon(data=bbox,aes(x=long, y = lat),color="red",fill=NA)+
      theme_classic()+xlab(NULL)+ylab(NULL)+
      coord_sf(xlim = c(180, 260), ylim = c(10,62),expand=F)+
      ggtitle(glue("{save_name} {get_date}"))
    
    png(glue("{benioffdir}/{write_out_name}_{get_date}.png"),width=32,height=22,units='cm',res=400,type = "cairo")
    par(ps=10)
    par(mar=c(4,4,1,1))
    par(cex=1)
    print({plot_sp})
    # gg_hm
    dev.off()
  }
 
  if(length(list.files(finaldir))==32){
  ############ 4. define global objects
  template=raster(glue("{path}/static_variables/template.grd"))
  species=c("albacoretuna_TOPP","black-footedAlbatross_Dallas","blueShark_TOPP",              
 "blueWhale_TOPP","californiaSeaLion_TOPP","elephantSeal_TOPP",           
 "laysanAlbatross_Dallas","leatherbackTurtle_TOPP","makoShark_TOPP",              
  "pacificBluefinTuna_TOPP","salmonShark_TOPP","sootyShearwater_TOPP",        
  "whiteShark_TOPP","yellowfinTuna_TOPP")
  
  co_stack=list.files(glue("{envdir}/{get_date}"),pattern=".grd",full.names=T) %>% 
    stack()
  co_names=list.files(glue("{envdir}/{get_date}"),pattern=".grd",full.names=F) %>% 
    gsub(".grd","",.)
  names(co_stack)=co_names
  
  ############ 5. predict models
  print("**************************************************************************************")
  print(paste0("Starting script for predicting models,"," Time is ",Sys.time()))
  
  predict_models(sp_name = "albacoretuna_TOPP")
  make_png(sp_name = "albacoretuna_TOPP",save_name="Albacore tuna")
  
  predict_models(sp_name = "blueShark_TOPP")
  make_png(sp_name = "blueShark_TOPP",save_name="Blue shark")
  
  predict_models(sp_name = "makoShark_TOPP")
  make_png(sp_name = "makoShark_TOPP",save_name="Mako shark")
  
  predict_models(sp_name = "pacificBluefinTuna_TOPP")
  make_png(sp_name = "pacificBluefinTuna_TOPP",save_name="Bluefin tuna")
  
  predict_models(sp_name = "salmonShark_TOPP")
  make_png(sp_name = "salmonShark_TOPP",save_name="Salmon shark")
  
  predict_models(sp_name = "whiteShark_TOPP")
  make_png(sp_name = "whiteShark_TOPP",save_name="White shark")
  
  predict_models(sp_name = "yellowfinTuna_TOPP")
  make_png(sp_name = "yellowfinTuna_TOPP",save_name="Yellowfin tuna")
  
  ###
  
  predict_models(sp_name = "black-footedAlbatross_Dallas")
  make_png(sp_name = "black-footedAlbatross_Dallas",save_name="Black-footed albatross")
  
  predict_models(sp_name = "blueWhale_TOPP")
  make_png(sp_name = "blueWhale_TOPP",save_name="Blue whale")
  
  predict_models(sp_name = "sootyShearwater_TOPP")
  make_png(sp_name = "sootyShearwater_TOPP",save_name="Sooty shearwater")
  
  predict_models(sp_name = "elephantSeal_TOPP")
  make_png(sp_name = "elephantSeal_TOPP",save_name="Elephant seal")
  
  predict_models(sp_name = "californiaSeaLion_TOPP")
  make_png(sp_name = "californiaSeaLion_TOPP",save_name="California sea lion")
  
  predict_models(sp_name = "laysanAlbatross_Dallas")
  make_png(sp_name = "laysanAlbatross_Dallas",save_name="Laysan albatross")
  
  predict_models(sp_name = "leatherbackTurtle_TOPP")
  make_png(sp_name = "leatherbackTurtle_TOPP",save_name="Leatherback turtle")
  }    
      print("**************************************************************************************")
  # close(logfile)
  sink(NULL)
}

predict_models(path=path,source_path=source_path,alb_path=alb_path)
