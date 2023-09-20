#######create latest files for Dale and Benioff 
#adapted from EcoCast by Heather Welch (NOAA/UCSC) 

############# ----------------------------> Paths to change ################
## path to the top directory of this project - doesn't matter what this folder is called, but something in /home/hwelch
path="/Users/heatherwelch/Dropbox/benioff_operationalization/operationalization_NOAA"

## path to the load libraries r script ("/loadlib-new.R") will be appended to this path in the function)
source_path="/Users/heatherwelch/Dropbox/benioff_operationalization/BlueWhaleROMS/Operationalizing/V2_NOAA"
############# ----------------------------> End ################

create_latest=function(path,source_path){
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
  
  ### updating the latest files in case the lastest was created by one of the batch scripts instead of the daily script ####
  mean=list.files(latestdir,full.names = T)
  mean=mean[!file.info(mean)$isdir]
  lapply(mean,function(x)file.remove(x))
  
  ## update files in latest folder
  last_date_w_data=list.files(flyersdir) %>% 
    gsub("blwh_flyer_","",.)%>% 
    gsub(".png","",.) %>% 
    lapply(., function(x)as.Date(x)) %>% 
    do.call("c",.)%>%  max() %>% as.character()
  
  ## flyer
  flyer=glue("{flyersdir}/blwh_flyer_{last_date_w_data}.png")
  rastergri=glue("{rastersdir}/blwh_ensemble_{last_date_w_data}.gri")
  rastergrd=glue("{rastersdir}/blwh_ensemble_{last_date_w_data}.grd")
  map=glue("{mapssdir}/blwh_ensemble_{last_date_w_data}.png")
  
  filesToMove=list(flyer,rastergri,rastergrd,map)
  lapply(filesToMove,function(x) file.copy(x,latestdir)) ## copy and then rename files
  file.rename(glue("{latestdir}/blwh_ensemble_{last_date_w_data}.grd"),glue("{latestdir}/blwh_ensemble_latest.grd")) ## maintaining old names so we can still match up to dropbox folder containing readme that explains how these files were made
  file.rename(glue("{latestdir}/blwh_ensemble_{last_date_w_data}.gri"),glue("{latestdir}/blwh_ensemble_latest.gri"))
  file.rename(glue("{latestdir}/blwh_ensemble_{last_date_w_data}.png"),glue("{latestdir}/blwh_ensemble_latest.png"))
  file.rename(glue("{latestdir}/blwh_flyer_{last_date_w_data}.png"),glue("{latestdir}/blwh_flyer_latest.png"))
  lapply(filesToMove,function(x) file.copy(x,latestdir)) ## copy and then don't rename files
  
  ### latest week stuff ####
  list.files(latestweekdir,full.names = T) %>% file.remove(.)

  print("1")
  files=list.files(flyersdir,full.names = T)
  if(length(files)<8){
    subsett=files[1:(length(files))] 
  } else (subsett=files[(length(files)-7):(length(files))])
  
  lapply(subsett,function(x) file.copy(subsett,latestweekdir))
  
  print("2")
  oldnames=rev(list.files(latestweekdir))
  oldnames_length=length(oldnames)
  oldnames_dates= oldnames%>% gsub("blwh_flyer_","",.)%>% 
    gsub(".png","",.) 
  
  newnames=list()
  for(i in 1:oldnames_length){
      name=glue("blwh_flyer_latest_{i}.png")
      newnames[[length(newnames)+1]]=name
  }
  newnames1=unlist(newnames)
  file.rename(from = file.path(glue("{latestweekdir}"), oldnames), to = file.path(glue("{latestweekdir}"), newnames1))
  print("3")
  
  ## smoothed stuff ####
  list.files(latestweeksmootheddir,full.names = T) %>% file.remove(.)
  files=list.files(rastersdir,full.names = T,pattern="tif")
  # files_nopath=list.files(rastersdir,full.names = F,pattern=".tif")
  if(length(files)<8){
    subsett=files[1:(length(files))] 
  } else (subsett=files[(length(files)-7):(length(files))])
  
  lapply(subsett,function(x) file.copy(subsett,latestweeksmootheddir))
  

  # for (i in 1:length(subsett)){
  #   ras=raster(subsett[i])
  #   smooth_r <- disaggregate(ras, 5) 
  #   smooth_r1=smooth_r %>% focal(w = matrix(1/25,5,5), FUN = mean, na.rm = T, pad = T)
  #   newname=glue("{latestweeksmootheddir}/{files_nopath[i]}") %>% 
  #     gsub(".grd",".tif",.)
  #   writeRaster(smooth_r1, newname, overwrite = T,format="GTiff")
  # }

}


create_latest(path=path,source_path=source_path)
