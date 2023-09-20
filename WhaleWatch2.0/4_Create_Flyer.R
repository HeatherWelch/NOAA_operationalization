# script to predicts models: gams and BRTS
# adapted from EcoCast and EcoROMS and BlueWhaleROMS by Heather Welch (UCSC/NOAA)

############# ----------------------------> Paths to change ################
## path to the top directory of this project - doesn't matter what this folder is called, but something in /home/hwelch
path="/Users/heatherwelch/Dropbox/benioff_operationalization/operationalization_NOAA"

## path to the load libraries r script ("/loadlib-new.R") will be appended to this path in the function)
source_path="/Users/heatherwelch/Dropbox/benioff_operationalization/BlueWhaleROMS/Operationalizing/V2_NOAA"
############# ----------------------------> End ################

create_flyer=function(path,source_path){
  
  ############ 1. Define directories
  
  # source(paste0(source_path,"load_libraries.R"),chdir=T)
  source(paste0(source_path,"/loadlib-new.R"),chdir=T)

  envdir=glue("{path}/daily_prediction_layers")
  outdir <- glue("{path}/BenioffRuns")
  staticdir=glue("{path}/static_variables")
  temp=glue("{path}/raw_roms_data")
  moddir=glue("{path}/models")
  intermediatedir=glue("{path}/intermediate")
  logodir=glue("{path}/logo")
  
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
  make_png_flyer=function(r,get_date,savedir, xlim=c(-130,-115.5),ylim=c(30,47), width=6.5, height=7, shiplane=FALSE,latest=F){

    if(latest){
      png(glue("{savedir}/blwh_int_flyer_latest.png"), width=width, height=height, units="in", res=400)
    }else{png(glue("{savedir}/blwh_int_flyer_{get_date}.png"), width=width, height=height, units="in", res=400)}
  
  par(ps=10) #settings before layout
  layout(matrix(c(1,1), nrow=1, ncol=1, byrow=TRUE))
  #layout.show(2) # run to see layout; comment out to prevent plotting during .pdf
  par(cex=1) # layout has the tendency change par()$cex, so this step is important for control

  par(mar=c(2,2,1,1)) # set margins before each plot
  #pal <- colorRampPalette(c("blue", "grey", "red"))
  pal <- colorRampPalette(c("purple4","blue", "cyan", "yellow", "red"))
  #pal <- colorRampPalette(c("purple4", "white", "blue"))
  ncolors <- 100
  breaks <- seq(0,1,length.out=ncolors+1)
  image.plot(r, col=pal(ncolors), breaks=breaks, ylab="", xlab="", xlim=xlim,ylim=ylim,legend.shrink = 0.9, legend.width = 1.2)
  maps::map('worldHires',add=TRUE,col=grey(0.7),fill=TRUE)
  corners = par("usr") #Gets the four corners of plot area (x1, x2, y1, y2)
  par(xpd = TRUE) #Draw outside plot area
  text(x = corners[2]+2.5, y = mean(corners[3:4]), "Probability of blue whale presence", srt = 270)
  text(-122,46,format(get_date,format="%b %d %Y"),adj=c(0,0),cex=2) 
# contour(r, add=TRUE, col="black",levels=c(.5,.75))
if(shiplane) {
  plot(insidelaneshp, add=T, lwd=2)
  plot(outsidelaneshp, add=T, lwd=2)
}
box()
dev.off()
}

  ############ 4. create output
  print("**************************************************************************************")
  print(paste0("Starting script 4_Create_Flyer,"," Time is ",Sys.time()))
  if(length(list.files(finaldir))==36){
  r=raster(glue("{latestdir}/blwh_ensemble_latest.grd"))
  make_png_flyer(r=r,get_date=get_date,savedir = intermediatedir, shiplane=F,latest = F)
  make_png_flyer(r=r,get_date=get_date,savedir = intermediatedir, shiplane=F,latest = T)
  
  ############ 8. make final product with metadata
  template=image_read(glue("{logodir}/template_new.png"))
  benioff=image_read(glue("{intermediatedir}/blwh_int_flyer_latest.png"))
  # b2=image_scale(benioff,"970")
  #c=image_crop(benioff,"827x1100+17-0") 
  template2=image_scale(template, "2800")
  # a=image_composite(template2,benioff,offset = "+60+420")
  a=image_composite(template2,benioff,offset = "+60+860")
  b=image_annotate(a,paste0("Image created ",Sys.Date()," by HW. Next projected image date: ",Sys.Date()+1),size=30,gravity = "southeast",location="+560+830",font = "courier")
  image_write(b,glue("{latestdir}/blwh_flyer_latest.png"))
  image_write(b,glue("{flyersdir}/blwh_flyer_{get_date}.png"))
  print("**************************************************************************************")
  }

}

create_flyer(path=path,source_path=source_path)


