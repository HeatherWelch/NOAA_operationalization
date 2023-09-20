#######Get_Env_Data A - BENIOFF
# Get data sequence number one : Create final and temporary envdirs, acquire all static variables
# ONLY RUN ONCE AT BEGINNING OF DAY
#adapted from EcoCast by Heather Welch (NOAA/UCSC) ds

############# ----------------------------> Paths to change ################
## path to the top directory of this project - doesn't matter what this folder is called, but something in /home/hwelch
path="/Users/heatherwelch/Dropbox/OLE/operationalization_albacore"

## path to the load libraries r script ("/loadlib-new-albacore.R") will be appended to this path in the function)
source_path="/Users/heatherwelch/Dropbox/OLE/github/OLE_Projects_new/scratch_workspaces/scratch_heather/10a_operationalization_albacore/V2"
############# ----------------------------> End ################

Get_Env_Data_A <- function(path,source_path){
  
  ############ 1. Define directories
   source(paste0(source_path,"/loadlib-new-albacore.R"),chdir=T)

    envdir=glue("{path}/daily_prediction_layers")
    outdir <- paste(path,"/model_runs/",sep="")
    logdir=paste(outdir,"logs/",sep="")
    staticdir=glue("{path}/static_variables/")
    benioffdir=glue("{path}/shiny_www")

  
  ############ 2. Define time and dynamic directories
   get_date=Sys.Date()
   # get_date="2022-07-23"
  
  finaldir=glue("{envdir}/{get_date}") #change for each user
  if(!file.exists(finaldir)){
    dir.create(finaldir)
  }
  
  
  # C. Set up logfile
  templogfile = paste(logdir,"log_",get_date,".txt",sep="") 
  # Sys.umask("006") ## making the file readable and writable by owner and group
  logfile <- file(templogfile, open="wt")
  sink(logfile, type=c("output","message")) #set all output to templog
  
  print("**************************************************************************************")
  print(paste("Starting OLE run for ",get_date,". Time is ",Sys.time(),sep=""))
  
  ############ 3. Define global objects
  template=raster(glue("{path}/static_variables/template.grd"))
  
  ################### Acquire static variables
  print("Starting script Env_Data_A.R")
  
  ## bathy and bathy sd are already smoothed by 1.25
  files=as.list(c("bathy.grd","bathy_sd.grd","bathy.gri","bathy_sd.gri","hooks.gri","hooks.grd","random.gri","random.grd","hbf.gri","hbf.grd","flt.gri","flt.grd"))
  lapply(files,function(x) file.copy(paste(staticdir,x,sep=""),finaldir))

  print(paste("Calculating day of year for ",get_date,sep=""))
  day=template
  daylight=yday(get_date)
  day[]=daylight
  writeRaster(day,paste(finaldir,"/day",sep=""),overwrite=T)
  
  #warnings()
  print("**************************************************************************************")
  close(logfile)
  
}

Get_Env_Data_A(path=path,source_path = source_path)

