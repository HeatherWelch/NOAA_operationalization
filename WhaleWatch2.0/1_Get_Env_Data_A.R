#######Get_Env_Data A - BENIOFF
# Get data sequence number one : Create final and temporary envdirs, acquire all static variables
# ONLY RUN ONCE AT BEGINNING OF DAY
#adapted from EcoCast by Heather Welch (NOAA/UCSC) 

############# ----------------------------> Paths to change ################
## path to the top directory of this project - doesn't matter what this folder is called, but something in /home/hwelch
path="/Users/heatherwelch/Dropbox/benioff_operationalization/operationalization_NOAA"

## path to the load libraries r script ("/loadlib-new.R") will be appended to this path in the function)
source_path="/Users/heatherwelch/Dropbox/benioff_operationalization/BlueWhaleROMS/Operationalizing/V2_NOAA"
############# ----------------------------> End ################

Get_Env_Data_A <- function(path,source_path){
  
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

  tmpdir=glue("{temp}/temp_",get_date,sep="")
  if(!file.exists(tmpdir)){
    dir.create(tmpdir) #create a temp directory to hold unfinished layers, change for each user
  }
  
  finaldir=glue("{envdir}/{get_date}") #change for each user
  if(!file.exists(finaldir)){
    dir.create(finaldir)
  }
  
  print("**************************************************************************************")
  print(paste("Starting Benioff run for ",get_date,". Time is ",Sys.time(),sep=""))
  
  ############ 3. Define global objects
  template=raster(glue("{path}/static_variables/template.grd"))
  
  ################### Acquire static variables
  print("Starting script Env_Data_A.R")
  
  files=as.list(c("z_.1.grd","zsd_1.grd","z_.1.gri","zsd_1.gri"))
  lapply(files,function(x) file.copy(glue("{staticdir}/{x}"),finaldir))
  file.rename(paste0(finaldir,"/z_.1.grd"),paste0(finaldir,"/z.grd")) ## maintaining old names so we can still match up to dropbox folder containing readme that explains how these files were made
  file.rename(paste0(finaldir,"/zsd_1.grd"),paste0(finaldir,"/z_sd.grd"))
  file.rename(paste0(finaldir,"/z_.1.gri"),paste0(finaldir,"/z.gri"))
  file.rename(paste0(finaldir,"/zsd_1.gri"),paste0(finaldir,"/z_sd.gri"))
  
  files=as.list(c("aspect_0.1.tif","slope_0.1.tif"))
  lapply(files,function(x) file.copy(glue("{staticdir}/{x}"),finaldir))
  
  xy=coordinates(template); lat=template; lat[]=xy[,2]      
  writeRaster(lat,paste(finaldir,"/lat",sep=""),overwrite=T)
  
  xy=coordinates(template); lon=template; lon[]=xy[,1]      
  writeRaster(lon,paste(finaldir,"/lon",sep=""),overwrite=T)
  
  ############ 5. lunillum
  print(paste("Calculating lunillum for ",get_date,sep=""))
  value <- lunar.illumination(as.Date(get_date))
  lunar_ras=template
  values(lunar_ras)=value
  writeRaster(lunar_ras,paste(finaldir,"/lunillum",sep=""),overwrite=T)
  
  ############ 6. wipe files in /latest
  # mean=list.files(paste0(benioffdir,"latest/"),full.names = T)
  # lapply(mean,function(x)file.remove(x))
  
  # se=list.files(paste0(benioffdir,"brt/se/latest/"),full.names = T)
  # lapply(se,function(x)file.remove(x))
  
  #warnings()
  print("**************************************************************************************")
  
}

Get_Env_Data_A(path=path,source_path = source_path)
