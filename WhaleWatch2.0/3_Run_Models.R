# script to predicts models: gams and BRTS
# adapted from EcoCast and EcoROMS and BlueWhaleROMS by Heather Welch (UCSC/NOAA)

############# ----------------------------> Paths to change ################
## path to the top directory of this project - doesn't matter what this folder is called, but something in /home/hwelch
path="/Users/heatherwelch/Dropbox/benioff_operationalization/operationalization_NOAA"

## path to the load libraries r script ("/loadlib-new.R") will be appended to this path in the function)
source_path="/Users/heatherwelch/Dropbox/benioff_operationalization/BlueWhaleROMS/Operationalizing/V2_NOAA"
############# ----------------------------> End ################

predict_models=function(path,source_path){
  
  ############ 1. Define directories
  
  # source(paste0(source_path,"load_libraries.R"),chdir=T)
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
  
  
  ############ 2. Define time and dynamic directories
  get_date=Sys.Date()-1
  # get_date="2019-12-14"
  
  finaldir=glue("{envdir}/{get_date}")

  ############ 3. define functions
  make_png_operationalization=function(r,get_date,savedir,type, xlim=c(-130,-115.5),ylim=c(30,47), width=5, height=7, shiplane=FALSE,latest=F){
  
    if(latest){
      png(glue("{savedir}/blwh_{type}_latest.png"), width=width, height=height, units="in", res=400)
    }else{png(glue("{savedir}/blwh_{type}_{get_date}.png"), width=width, height=height, units="in", res=400)}
  par(ps=10) #settings before layout
  layout(matrix(c(1,2), nrow=2, ncol=1, byrow=TRUE), heights=c(4,1), widths=height)
  #layout.show(2) # run to see layout; comment out to prevent plotting during .pdf
  par(cex=1) # layout has the tendency change par()$cex, so this step is important for control
  
  par(mar=c(4,4,1,1)) # set margins before each plot
  #pal <- colorRampPalette(c("blue", "grey", "red"))
  pal <- colorRampPalette(c("purple4","blue", "cyan", "yellow", "red"))
  #pal <- colorRampPalette(c("purple4", "white", "blue"))
  ncolors <- 100
  breaks <- seq(0,1,length.out=ncolors+1)
  image(r, col=pal(ncolors), breaks=breaks, ylab="", xlab="", xlim=xlim,ylim=ylim)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  # contour(r, add=TRUE, col="black",levels=c(.5,.75))
  if(shiplane) {
    plot(insidelaneshp, add=T, lwd=2)
    plot(outsidelaneshp, add=T, lwd=2)
  }
  box()
  
  par(mar=c(4,4,0,1)) # set margins before each plot
  levs <- breaks[-1] - diff(breaks)/2
  image(x=levs, y=1, z=as.matrix(levs), col=pal(ncolors), breaks=breaks, ylab="", xlab="", yaxt="n")
  mtext(paste0("Probability of blue whale presence ",get_date," (",type,")",sep=" "), side=1, line=2.5)
  
  box()
  
  dev.off() # closes device
}
  
  pred_ensemble_ROMS<-function(get_date,GAMM_ws, BRT_ws, GAMM_sf,BRT_sf, stack,studyarea,template,outdir_pred,xlim=c(-130,-115.5),ylim=c(30,47),width=5, height=7, shiplane=FALSE){
    
    stack=as.data.frame(stack,stringsAsFactors=F) ## predicts on data.frame, not on a stack
    stack$ptt = 723029
    
    #assign winter/spring model weightings for each week of year
    #weightings for summer/fall model are 1 - winter/spring weightings
    ws_weightings <- c(rep(1,22), .8,.6,.4,.2, rep(0,17),.2,.4,.6,.8, rep(1,6)) 
    
    ws_GAMM_pred <- predict.gam(GAMM_ws,newdata=stack, type = 'response')
    ws_BRT_pred <- predict.gbm(BRT_ws,newdata=stack,n.trees=1000,type='response')
    sf_GAMM_pred <- predict.gam(GAMM_sf,newdata=stack, type = 'response')
    sf_BRT_pred <- predict.gbm(BRT_sf,newdata=stack,n.trees=1000,type='response')
    
    ws_GAMM_ras<- setValues(template,as.numeric(ws_GAMM_pred))%>%mask(.,studyarea)
    ws_BRT_ras<- setValues(template,as.numeric(ws_BRT_pred))%>%mask(.,studyarea)
    sf_GAMM_ras<- setValues(template,as.numeric(sf_GAMM_pred))%>%mask(.,studyarea)
    sf_BRT_ras<- setValues(template,as.numeric(sf_BRT_pred))%>%mask(.,studyarea)
    
    ## write individual model rasters 
    print("writing out individual model rasters and PNGS")
    writeRaster(ws_GAMM_ras,glue("{intermediatedir}/blwh_ws_GAMM_{get_date}"),overwrite=T)
    writeRaster(ws_BRT_ras,glue("{intermediatedir}/blwh_ws_BRT_{get_date}"),overwrite=T)
    writeRaster(sf_GAMM_ras,glue("{intermediatedir}/blwh_sf_GAMM_{get_date}"),overwrite=T)
    writeRaster(sf_BRT_ras,glue("{intermediatedir}/blwh_sf_BRT_{get_date}"),overwrite=T)
    
    ## write ensemble PNGS
    make_png_operationalization(r=ws_GAMM_ras,get_date=get_date,savedir = glue("{intermediatedir}/"),type="ws_GAMM", latest = F)
    make_png_operationalization(r=ws_BRT_ras,get_date=get_date,savedir = glue("{intermediatedir}/"),type="ws_BRT", latest = F)
    make_png_operationalization(r=sf_GAMM_ras,get_date=get_date,savedir = glue("{intermediatedir}/"),type="sf_GAMM", latest = F)
    make_png_operationalization(r=sf_BRT_ras,get_date=get_date,savedir = glue("{intermediatedir}/"),type="sf_BRT", latest = F)
    
    ws_mod_preds <- cbind(ws_GAMM_pred, ws_BRT_pred); ws_mod_preds <- rowMeans(ws_mod_preds, na.rm=T)
    sf_mod_preds <- cbind(sf_GAMM_pred, sf_BRT_pred); sf_mod_preds <- rowMeans(sf_mod_preds, na.rm=T)
    
    # combine model predictions by weightings
    mod_preds <- (ws_mod_preds * ws_weightings[week(get_date)]) + (sf_mod_preds * (1-ws_weightings[week(get_date)]))
    
    ## make rasters 
    meanPredR <- setValues(template,mod_preds)%>%mask(.,studyarea)
    
    ## write ensemble rasters 
    print("writing out ensemble model rasters and PNGS")
    writeRaster(meanPredR,glue("{rastersdir}/blwh_ensemble_{get_date}"),overwrite=T)
    writeRaster(meanPredR,glue("{latestdir}/blwh_ensemble_latest"),overwrite=T)
    
    ## write out ensemble tiffs
    smooth_r <- disaggregate(meanPredR, 5) 
    smooth_r1=smooth_r %>% focal(w = matrix(1/25,5,5), FUN = mean, na.rm = T, pad = T)
    newname=glue("{rastersdir}/blwh_ensemble_{get_date}.tif") %>% 
      gsub(".grd",".tif",.)
    writeRaster(smooth_r1, newname, overwrite = T,format="GTiff")
    
    ## write ensemble PNGS
    make_png_operationalization(r=meanPredR,get_date=get_date,savedir = glue("{mapssdir}/"),type="ensemble", latest = F)
    make_png_operationalization(r=meanPredR,get_date=get_date,savedir = glue("{latestdir}/"),type="ensemble", latest = T)
  }

  if(length(list.files(finaldir))==36){
  ############ 4. define global objects
  template=raster(glue("{path}/static_variables/template.grd"))
  studyarea=st_read(glue("{staticdir}/sa_square_coast3.shp"))
  studyarea <- sf:::st_zm(studyarea$geom)
  studyarea=as(studyarea, "Spatial")
  
  WP=st_read(glue("{staticdir}/shp/WesternPolygon.shp"))
  WP <- sf:::st_zm(WP$geom)
  WP=as(WP, "Spatial")
  outsidelaneshp <-spTransform(WP, CRS("+proj=longlat +datum=WGS84")) #convert from UTM to LatLon
  TSS=st_read(glue("{staticdir}/shp/TSSpolygon.shp"))
  TSS <- sf:::st_zm(TSS$geom)
  TSS=as(TSS, "Spatial")
  insidelaneshp <-spTransform(TSS, CRS("+proj=longlat +datum=WGS84")) #convert from UTM to LatLon
  
  brt_covars=c("sst.grd","z_sd.grd","z.grd","ild.grd","slope_0.1.tif","sst_sd.grd","ssh.grd","bv.grd","ssh_sd.grd","aspect_0.1.tif","EKE.grd","curl.grd")
  brt_stack=lapply(brt_covars,function(x)glue("{finaldir}/{x}")) %>% unlist() %>% stack()
  names(brt_stack)=c("sst_mean_0.1","zsd_1","z_0.1","ild_mean_0.1","slope","sst_sd_1","ssh_mean_0.1","BV_frequency_mean_0.1","ssh_sd_1","aspect","EKE_0.1","curl_mean_0.5")
  
  gam_covars=c("sst.grd","z_sd.grd","z.grd","ild.grd","ssh_sd.grd","EKE.grd") ## this isn't needed but leaving it in case. brt_stack can be used for brts and gamms
  gam_stack=lapply(gam_covars,function(x)glue("{finaldir}/{x}")) %>% unlist() %>% stack()
  names(gam_stack)=c("sst_mean_0.1","zsd_1","z_0.1","ild_mean_0.1","ssh_sd_1","EKE_0.1")
  
  BRT_ws=readRDS(glue("{moddir}/blwh.res1.tc3.lr05.single.DecJun.final.rds"))
  BRT_sf=readRDS(glue("{moddir}/blwh.res1.tc3.lr05.single.JulNov.final.rds"))
  GAMM_ws=readRDS(glue("{moddir}/blwh.res1.gam.ws.mod1.rds"))
  GAMM_sf=readRDS(glue("{moddir}/blwh.res1.gam.sf.mod1.rds"))
  
  ############ 5. predict models
  print("**************************************************************************************")
  print(paste0("Starting script for predicting models,"," Time is ",Sys.time()))
  pred_ensemble_ROMS(get_date = get_date,GAMM_ws = GAMM_ws,BRT_ws=BRT_ws,GAMM_sf=GAMM_sf,BRT_sf=BRT_sf,stack=brt_stack,studyarea = studyarea,template = template,outdir = outdir,shiplane=T)
  }    
      print("**************************************************************************************")
}

predict_models(path=path,source_path=source_path)
