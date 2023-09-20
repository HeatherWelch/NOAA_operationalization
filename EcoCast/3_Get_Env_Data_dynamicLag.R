#######Get_Env_Data_B_1
# Get data sequence number two (for preceeding days to get_date)
#This script serves to check online for available online data for up to get_date - 3
#Some times preceeding days data won't be available locally because it wasn't available for download when the original 2_Get_Env_data_B.R was run 
#Important to check if now available online so that EcoCast for get_date doesn't needlessly exlcude variable

#Script only checks for new data online if data in local folders are missing
#RUN THIS SCRIPT ONCE IN THE MORNING AND ONCE SOMETIME IN THE AFTERNOON

############# ----------------------------> Paths to change ################
## path to the top directory of this project - doesn't matter what this folder is called, but something in /home/hwelch
path="/Users/heatherwelch/Dropbox/Ecocast/operationalization_ecocast"

## path to the load libraries r script (path to script only, /0_load_libraries.R appended in function)
source_path="/Users/heatherwelch/Dropbox/Ecocast/EcoCast-private/Code/Operationalizing_code_V4"

Get_Env_Data_B_1=function(path,source_path){
  
  ############ 1. Define directories
  source(paste0(source_path,"/0_load_libraries.R"))
  source(glue("{source_path}/predict_CIs_fcn.R"))
  source(glue("{source_path}/plot_EcoCast_fcn.R"))
  envdir=paste(path,"/SpatialPredictions_EnvData/Satellite/",sep="") 
  outdir <- paste(path,"/EcoCastRuns/",sep="")
  logdir=paste(outdir,"logs/",sep="")
  staticdir=paste0(path,"/static_variables/")
  ecocastdir=paste(outdir,"output/",sep="")
  moddir<-paste(path,"/ModRepFiles/",sep="")
  
############ 2. Define functions
### A. Pauses system for a period of time to allow url requests to go through
waitfor <- function(x){
  p1 <- proc.time()
  Sys.sleep(x)
  print(proc.time() - p1) # The cpu usage should be negligible
}

### C. Checks if a URL exists and returns true or false
URLexists=function(urls){
  b=url.exists(urls)
  return(b)
}

### D. An acquire function to grab envt data from erddap
acquire_erddap=function(urls,name,final_name){ #name is for the variable name in ERDDAP, final_name is for the final processed layer (e.g. l.blendchla )
  envir = parent.frame()
  presence=URLexists(urls=urls)
  if(URLexists(urls=urls)==TRUE){
    file = paste(tmpdir,"/",name,".nc",sep="")
    print(paste("Beginning download of ",name,". Placing it in a temp directory: ",tmpdir,sep=""))
    f = CFILE(file,mode="wb")
    curlPerform(url=urls,writedata=f@ref,noprogress=FALSE)
    close(f)
    waitfor(3)
  }
}

# ### E. An acquire function to grab envt data from CMEMS and AVISO (these have a slightly different format than erddap and therefore need a seperate download method) ### DEPRECIATED
# acquire_cmems_aviso=function(url,date,userpwd,name){ #name is for the variable name in ERDDAP, final_name is for the final processed layer (e.g. l.blendchla )
#   filenames=getURL(url, userpwd = userpwd,
#                    ftp.use.epsv = FALSE,ssl.verifypeer = FALSE,dirlistonly = TRUE) ## this is clunky by necessity. The CMEMS files are named by the date they were uploaded to the ftp site, therefore there is no way to predict the actual name of the file for the date we are interested in. So we go a roundabout way:
#   waitfor(3)
#   list_filenames=unlist(strsplit(filenames,".gz")) ## get a list of all the files in the CMEMS directory
#   string=grep(date,list_filenames,value=TRUE)
#   if(length(string)>0){
#     string=gsub("[^[:alnum:]_.]", "", string) ## it is impossible to get rid of trailing backslashes, therefore this mess
#     data=getBinaryURL(paste(url,string,".gz",sep=""),userpwd = userpwd,ftp.use.epsv = FALSE,ssl.verifypeer = FALSE,noprogress=FALSE) # grab data behind url
#     waitfor(3)
#     con <- file(paste(tmpdir,"/",name,".nc.gz",sep=""), open = "wb") # write data to a file
#     writeBin(data,con)
#     waitfor(3)
#     close(con)
#     gunzip(paste(tmpdir,"/",name,".nc.gz",sep=""),ext="gz", FUN=gzfile) # unzip the file
#   }
# }

### E. An acquire function to grab envt data from CMEMS and AVISO (these have a slightly different format than erddap and therefore need a seperate download method) 
acquire_cmems_aviso=function(url,date,userpwd,name){ #name is for the variable name in ERDDAP, final_name is for the final processed layer (e.g. l.blendchla )
  filenames=getURL(url, userpwd = userpwd,
                   ftp.use.epsv = FALSE,ssl.verifypeer = FALSE,dirlistonly = TRUE) ## this is clunky by necessity. The CMEMS files are named by the date they were uploaded to the ftp site, therefore there is no way to predict the actual name of the file for the date we are interested in. So we go a roundabout way:
  waitfor(3)
  list_filenames=unlist(strsplit(filenames,".nc")) ## get a list of all the files in the CMEMS directory
  string=grep(date,list_filenames,value=TRUE)
  if(length(string)>0){
    string=gsub("[^[:alnum:]_.]","", string) ## it is impossible to get rid of trailing backslashes, therefore this mess
    data=getBinaryURL(paste(url,string,".nc",sep=""),userpwd = userpwd,ftp.use.epsv = FALSE,ssl.verifypeer = FALSE,noprogress=FALSE) # grab data behind url
    waitfor(3)
    con <- file(paste(tmpdir,"/",name,".nc",sep=""), open = "wb") # write data to a file
    writeBin(data,con)
    waitfor(3)
    close(con)
  }
}


############ 5. Define global objects
#these are grabbed from sla_mean.grd in /EcoCast_CodeArchive/SpatialPredictions_EnvData/Satellite/2012-08-04
template=raster() ##create template for resampling
res(template)=0.2487562
ncol(template)=201
nrow(template)=201
xmin(template)=-149.875
xmax(template)=-99.875
ymin(template)=10.125
ymax(template)=60.125
projection(template)="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

############ 2. Set up for loop
get_dateT=Sys.Date()
SEQNCE=unlist(list(as.character(get_dateT-1),as.character(get_dateT-2),as.character(get_dateT-3)))

for(get_date in SEQNCE){
  print(get_date)
  get_date=as.Date(get_date)
  get_date_composite=get_date-4
  year=get_date %>% as.character() %>% substr(.,start=1,stop=4)
  month=get_date %>% as.character() %>% substr(.,start=6,stop=7)
  tmpdir=paste(path,"/Real_time_netcdfs_raw/temp_",get_date,sep="")
  if(!file.exists(tmpdir)){
    dir.create(tmpdir)
  }
  finaldir=paste(envdir,get_date,sep="")
  if(!file.exists(finaldir)){
    dir.create(finaldir)
  }
  
  ############ 11.Get a list of the paths of the env variables for get_date, or the most recent path if missing
  FileList_get_date=list.files(paste(envdir,get_date,sep=""),pattern="*.grd$") # all the files from get_date
  FileList_full=c("analysed_sst.grd","analysed_sst_sd.grd","l.blendChl.grd","l.eke_mean.grd","sla.grd","sla_sd.grd","ywind.grd") # all of the dynamic variables, static ones will always be there
  FileList_missing=setdiff(FileList_full,FileList_get_date) # list of dynamic variables missing from get_date
  
  print("**************************************************************************************")
  #dynamic variables
  
  ############ 3. Variable 1,2, &3: NRT MSLA SSH and U&V (now all hosted within one netcdf)
  if(("sla.grd" %in% FileList_missing)==TRUE){
    print("Downloading and preparing NRT MSLA SSH")
    print("Downloading and preparing NRT MSLA u&v")
    date=paste("l4_",gsub("-","",get_date),sep="") # get date in correct format for ftp search
    url <- paste0("ftp://nrt.cmems-du.eu/Core/SEALEVEL_GLO_PHY_L4_NRT_OBSERVATIONS_008_046/dataset-duacs-nrt-global-merged-allsat-phy-l4","/",year,"/",month,"/")
    userpwd <- "CMEMESusername:CMEMESpassword"
    name="MSLA_all"
    acquire_cmems_aviso(url=url,date=date,userpwd=userpwd,name=name)
    ## process any new files
    if(file.exists(paste(tmpdir,"/MSLA_all.nc",sep="")) & !file.exists(paste0(finaldir,"/sla.grd"))){
      print(paste("Preparing MSLAh: standardizing extent and placing file in ",finaldir,sep=""))
      MSLAh=raster(paste(tmpdir,"/MSLA_all.nc",sep=""),varname="sla")
      r=raster::resample(MSLAh, template, method="bilinear")
      rsd=focal(r, w=matrix(1,nrow=7,ncol=7), fun=sd,na.rm=TRUE)
      writeRaster(r,paste(finaldir,"/sla",sep=""),overwrite=T)
      writeRaster(rsd,paste(finaldir,"/sla_sd",sep=""),overwrite=T)
    }
    if(file.exists(paste(tmpdir,"/MSLA_all.nc",sep="")) & !file.exists(paste0(finaldir,"/l.eke_mean.grd"))){
      print(paste("Preparing MSLAuv: standardizing extent, calculating l.eke and placing file in ",finaldir,sep=""))
      MSLAu=raster(paste(tmpdir,"/MSLA_all.nc",sep=""),varname="ugosa")
      MSLAv=raster(paste(tmpdir,"/MSLA_all.nc",sep=""),varname="vgosa")
      rU=raster::resample(MSLAu, template, method="bilinear") 
      rV=raster::resample(MSLAv, template, method="bilinear")
      eke<-1/2*(rU^2+rV^2)
      l.eke <- log(eke + 0.001)
      writeRaster(l.eke,paste(finaldir,"/l.eke_mean",sep=""),overwrite=T)
    }
  }
  
  ############ 5. Variable 4: Wind  --------------------------------> depreciated ####
  # if(("ywind.grd" %in% FileList_missing)==TRUE){
  #   wind=paste("http://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOwDlyP_LonPM180.nc?v[(",get_date_composite,"T00:00:00Z):1:(",get_date_composite,"T00:00:00Z)][(10):1:(10)][(10):1:(60)][(-150):1:(-100)]",sep="")
  #   name= "ncdcOwDly"
  #   final_name= "ywind"
  #   acquire_erddap(urls=wind,name=name,final_name=name)
  #   ## process any new files
  #   if(file.exists(paste(tmpdir,"/ncdcOwDly.nc",sep=""))==TRUE){
  #     print(paste("Preparing ncdcOwDly: standardizing extent and placing file in ",finaldir,sep=""))
  #     ncdcOwDly=raster(paste(tmpdir,"/ncdcOwDly.nc",sep=""),varname="v")
  #     r=raster::resample(ncdcOwDly, template, method="bilinear")
  #     writeRaster(r,paste(finaldir,"/ywind",sep=""))
  #   }
  # }
  ############  --------------------------------> depreciated ####
  
  ############ 5. Variable 4: Wind
  if(("ywind.grd" %in% FileList_missing)==TRUE){
    wind=paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdQMwind1day.nc?y_wind[(",get_date,"T00:00:00Z):1:(",get_date,"T00:00:00Z)][(10):1:(10)][(10):1:(60)][(210):1:(260)]",sep="")
    name= "erdQMwind1day"
    final_name= "ywind"
    acquire_erddap(urls=wind,name=name,final_name=name)
    ## process any new files
    if(file.exists(paste(tmpdir,"/erdQMwind1day.nc",sep=""))==TRUE){
      print(paste("Preparing erdQMwind1day: standardizing extent and placing file in ",finaldir,sep=""))
      erdQMwind1day=raster(paste(tmpdir,"/erdQMwind1day.nc",sep=""),varname="y_wind")
      r=raster::resample(rotate(erdQMwind1day), template, method="bilinear")
      writeRaster(r,paste(finaldir,"/ywind",sep=""))
    }
  }
  
  ############ 6. Variable 5: sst (GHRSST1)  --------------------------------> depreciated ####
  # if(("analysed_sst.grd" %in% FileList_missing)==TRUE){
  #   sst=paste("http://coastwatch.pfeg.noaa.gov/erddap/griddap/jplG1SST.nc?SST[(",get_date,"T00:00:00Z):1:(",get_date,"T00:00:00Z)][(10):1:(60)][(-150):1:(-100)]",sep="")
  #   name= "jplG1SST"
  #   acquire_erddap(urls=sst,name=name,final_name=name)
  # }
  ############ --------------------------------> depreciated ####
  
  # ############ Variable 5: sst (jplMURSST41)  --------------------------------> depreciated following emails with Scott and Gang ####
  # if(("analysed_sst.grd" %in% FileList_missing)==TRUE){ #only download if there is NOT a new jplG1SST layer 
  #   sst=paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(",get_date,"T00:00:00Z):1:(",get_date,"T00:00:00Z)][(10):1:(60)][(-150):1:(-100)]",sep="")
  #   name= "jplMURSST41"
  #   acquire_erddap(urls=sst,name=name,final_name=name)
  # }
  
  ############ Variable 5: sst (NOAA_DHW_5km) 
  if(("analysed_sst.grd" %in% FileList_missing)==TRUE){ #only download if there is NOT a new jplG1SST layer
    sst=paste("https://pae-paha.pacioos.hawaii.edu/erddap/griddap/dhw_5km.nc?CRW_SST[(",get_date,"T00:00:00Z):1:(",get_date,"T00:00:00Z)][(60):1:(10)][(-150):1:(-100)]",sep="")
    name= "NOAA_DHW"
    acquire_erddap(urls=sst,name=name,final_name=name)
  }
  
  ############ Variable 5.1: sst (noaacwBLENDEDsstDNDaily) ## commenting this out until I can figure out the problem
  # if(!file.exists(paste(tmpdir,"/NOAA_DHW.nc",sep="")) & ("analysed_sst.grd" %in% FileList_missing)==TRUE){ #only download if there is NOT a new jplG1SST layer
  #   sst=glue("https://coastwatch.noaa.gov//erddap/griddap/noaacwBLENDEDsstDNDaily.nc?analysed_sst%5B({get_date}T12:00:00Z):1:({get_date}T12:00:00Z)%5D%5B(9.025):1:(60.025)%5D%5B(-151.975):1:(-99.975)%5D")
  #   name= "noaacwBLENDEDsstDNDaily"
  #   acquire_erddap(urls=sst,name=name,final_name=name)
  # }
  # 
  # ############ Variable 5.1: sst (erdMWsstd1day_LonPM180) DEPRECIATED
  # if(!file.exists(paste(tmpdir,"/NOAA_DHW.nc",sep="")) & ("analysed_sst.grd" %in% FileList_missing)==TRUE){ #only download if there is NOT a new jplG1SST layer 
  #   sst=paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMH1sstd1day.nc?sst[(",get_date,"T00:00:00Z):1:(",get_date,"T00:00:00Z)][(60):1:(10)][(-150):1:(-100)]",sep="")
  #   name= "erdMH1sstd1day"
  #   acquire_erddap(urls=sst,name=name,final_name=name)
  # }
  
  ############ Variable 5.2: sst (jplUKMO_OSTIAv20)
  if(!file.exists(paste(tmpdir,"/erdMH1sstd1day.nc",sep="")) & !file.exists(paste(tmpdir,"/NOAA_DHW.nc",sep="")) & ("analysed_sst.grd" %in% FileList_missing)==TRUE){ #only download if there is NOT a new jplG1SST layer 
    sst=paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplUKMO_OSTIAv20.nc?analysed_sst[(",get_date,"T00:00:00Z):1:(",get_date,"T00:00:00Z)][(10):1:(60)][(-150):1:(-100)]",sep="")
    name= "jplUKMO_OSTIAv20"
    acquire_erddap(urls=sst,name=name,final_name=name)
  }
  
  ## process any new SST files  --------------------------------> depreciated ####
  # if(file.exists(paste(tmpdir,"/jplG1SST.nc",sep="")) & ("analysed_sst.grd" %in% FileList_missing)==TRUE){
  #   print("Preparing jplG1SST: regridding to .25 x .25 degree resolution")
  #   jplG1SST=raster(paste(tmpdir,"/jplG1SST.nc",sep=""),varname="SST")
  #   r=raster::resample(jplG1SST, template, method="bilinear")
  #   print(paste("jplG1SST regridding complete, placing final file in",finaldir,sep=""))
  #   writeRaster(r,paste(finaldir,"/analysed_sst",sep=""))
  #   print("Preparing jplG1SST: calculating standard deviation at native resolution")
  #   rsd=focal(jplG1SST, w=matrix(1,nrow=7,ncol=7), fun=sd,na.rm=TRUE)
  #   print("Preparing jplG1SST: regridding standard deviation layer to .25 x .25 degree resolution")
  #   rsdr=raster::resample(rsd, template, method="bilinear")
  #   print(paste("regridding complete, placing final file in",finaldir,sep=""))
  #   writeRaster(rsdr,paste(finaldir,"/analysed_sst_sd",sep=""))
  # }
  ## --------------------------------> depreciated ####
  
  if(file.exists(paste(tmpdir,"/NOAA_DHW.nc",sep="")) & ("analysed_sst.grd" %in% FileList_missing)==TRUE){
    print("Preparing NOAA_DHW: regridding to .25 x .25 degree resolution")
    jplG1SST=raster(paste(tmpdir,"/NOAA_DHW.nc",sep=""),varname="CRW_SST")
    r=raster::resample(jplG1SST, template, method="bilinear")
    print(paste("NOAA_DHW regridding complete, placing final file in",finaldir,sep=""))
    writeRaster(r,paste(finaldir,"/analysed_sst",sep=""))
    print("Preparing NOAA_DHW: calculating standard deviation at native resolution")
    rsd=focal(jplG1SST, w=matrix(1,nrow=7,ncol=7), fun=sd,na.rm=TRUE)
    print("Preparing NOAA_DHW: regridding standard deviation layer to .25 x .25 degree resolution")
    rsdr=raster::resample(rsd, template, method="bilinear")
    print(paste("regridding complete, placing final file in",finaldir,sep=""))
    writeRaster(rsdr,paste(finaldir,"/analysed_sst_sd",sep=""))
  }
  
  ## commenting this out until I can figure out the problem
  # if(file.exists(paste(tmpdir,"/noaacwBLENDEDsstDNDaily.nc",sep="")) & ("analysed_sst.grd" %in% FileList_missing)==TRUE){
  #   print("Preparing noaacwBLENDEDsstDNDaily: regridding to .25 x .25 degree resolution")
  #   noaacwBLENDEDsstDNDaily=raster(paste(tmpdir,"/noaacwBLENDEDsstDNDaily.nc",sep=""),varname="analysed_sst")
  #   r=raster::resample(noaacwBLENDEDsstDNDaily, template, method="bilinear")
  #   print(paste("noaacwBLENDEDsstDNDaily regridding complete, placing final file in",finaldir,sep=""))
  #   writeRaster(r,paste(finaldir,"/analysed_sst",sep=""))
  #   print("Preparing noaacwBLENDEDsstDNDaily: calculating standard deviation at native resolution")
  #   rsd=focal(noaacwBLENDEDsstDNDaily, w=matrix(1,nrow=7,ncol=7), fun=sd,na.rm=TRUE)
  #   print("Preparing noaacwBLENDEDsstDNDaily: regridding standard deviation layer to .25 x .25 degree resolution")
  #   rsdr=raster::resample(rsd, template, method="bilinear")
  #   print(paste("regridding complete, placing final file in",finaldir,sep=""))
  #   writeRaster(rsdr,paste(finaldir,"/analysed_sst_sd",sep=""))
  # }
  
  # if(file.exists(paste(tmpdir,"/erdMH1sstd1day.nc",sep="")) & ("analysed_sst.grd" %in% FileList_missing)==TRUE){ ### DEPRECIATED
  #   print("Preparing erdMH1sstd1day: regridding to .25 x .25 degree resolution")
  #   jplG1SST=raster(paste(tmpdir,"/erdMH1sstd1day.nc",sep=""),varname="sst")
  #   r=raster::resample(jplG1SST, template, method="bilinear")
  #   print(paste("erdMH1sstd1day regridding complete, placing final file in",finaldir,sep=""))
  #   writeRaster(r,paste(finaldir,"/analysed_sst",sep=""))
  #   print("Preparing erdMH1sstd1day: calculating standard deviation at native resolution")
  #   rsd=focal(jplG1SST, w=matrix(1,nrow=7,ncol=7), fun=sd,na.rm=TRUE)
  #   print("Preparing erdMH1sstd1day: regridding standard deviation layer to .25 x .25 degree resolution")
  #   rsdr=raster::resample(rsd, template, method="bilinear")
  #   print(paste("regridding complete, placing final file in",finaldir,sep=""))
  #   writeRaster(rsdr,paste(finaldir,"/analysed_sst_sd",sep=""))
  # }
  
  if(file.exists(paste(tmpdir,"/jplUKMO_OSTIAv20.nc",sep="")) & ("analysed_sst.grd" %in% FileList_missing)==TRUE){
    print("Preparing jplUKMO_OSTIAv20: regridding to .25 x .25 degree resolution")
    jplG1SST=raster(paste(tmpdir,"/jplUKMO_OSTIAv20.nc",sep=""),varname="analysed_sst")
    r=raster::resample(jplG1SST, template, method="bilinear")
    print(paste("jplUKMO_OSTIAv20 regridding complete, placing final file in",finaldir,sep=""))
    writeRaster(r,paste(finaldir,"/analysed_sst",sep=""))
    print("Preparing jplUKMO_OSTIAv20: calculating standard deviation at native resolution")
    rsd=focal(jplG1SST, w=matrix(1,nrow=7,ncol=7), fun=sd,na.rm=TRUE)
    print("Preparing jplUKMO_OSTIAv20: regridding standard deviation layer to .25 x .25 degree resolution")
    rsdr=raster::resample(rsd, template, method="bilinear")
    print(paste("regridding complete, placing final file in",finaldir,sep=""))
    writeRaster(rsdr,paste(finaldir,"/analysed_sst_sd",sep=""))
  }
  
  ############ 7. Variable 6: l.blendChl
  if(("l.blendChl.grd" %in% FileList_missing)==TRUE){
    chl=paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/nesdisVHNnoaaSNPPnoaa20chlaGapfilledDaily.nc?chlor_a%5B(",get_date,"T00:00:00Z):1:(",get_date,"T00:00:00Z)][(0):1:(0)][(60):1:(10)][(-150):1:(-100)]",sep="")
    name= "nesdisVHNnoaaSNPPnoaa20chlaGapfilledDaily"
    final_name="l.blendChl"
    acquire_erddap(urls=chl,name=name,final_name=final_name)
    
    ############ 8. Variable 7: nesdisVHNnoaaSNPPnoaa20chlaDaily
    if(file.exists(paste(tmpdir,"/nesdisVHNnoaaSNPPnoaa20chlaGapfilledDaily.nc",sep=""))==F){ #only download if there isn't a DINEOF layer
      chlVI=paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/nesdisVHNnoaaSNPPnoaa20chlaDaily.nc?chl_oci[(",get_date,"T00:00:00Z):1:(",get_date,"T00:00:00Z)][(0):1:(0)][(60):1:(10)][(-150):1:(-100)]",sep="")
      name= "nesdisVHNnoaaSNPPnoaa20chlaDaily"
      final_name= "erdVH3chla"
      acquire_erddap(urls=chlVI,name=name,final_name=final_name)
    }
    
    ############ 8. Variable 8: erdVHNchla1day
    if(file.exists(paste(tmpdir,"/nesdisVHNnoaaSNPPnoaa20chlaGapfilledDaily.nc",sep=""))==F & file.exists(paste(tmpdir,"/nesdisVHNnoaaSNPPnoaa20chlaDaily.nc",sep=""))==F){ #only download if there isn't a DINEOF or non-DINEOF layer
      chlVI2=paste("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdVHNchla1day.nc?chla[(",get_date,"T00:00:00Z):1:(",get_date,"T00:00:00Z)][(0):1:(0)][(60):1:(10)][(-150):1:(-110.00625)]",sep="")
      name= "erdVHNchla1day"
      final_name= "erdVHNchla1day"
      acquire_erddap(urls=chlVI2,name=name,final_name=final_name)
    }
    
    
    ## process any new files
    # Scenario 1: new layers for MODIS and VIIRS
    ## process any new files
    # Scenario 1: new layer for nesdisVHNnoaaSNPPnoaa20chlaGapfilledDaily
    if(file.exists(paste(tmpdir,"/nesdisVHNnoaaSNPPnoaa20chlaGapfilledDaily.nc",sep=""))==TRUE){
      print("Preparing nesdisVHNnoaaSNPPnoaa20chlaGapfilledDaily")
      chla=raster(paste(tmpdir,"/nesdisVHNnoaaSNPPnoaa20chlaGapfilledDaily.nc",sep=""),varname="chlor_a")
      print("Preparing erdMBchla: regridding to .25 x .25 degree resolution and taking the log")
      r=log(raster::resample(chla, template, method="bilinear")+0.001)
      print(paste("nesdisVHNnoaaSNPPnoaa20chlaGapfilledDaily regridding complete, placing final file in",finaldir,sep=""))
      writeRaster(r,paste(finaldir,"/l.blendChl",sep=""),overwite=T)
    }
    
    # Scenario 2: new layer for nesdisVHNnoaaSNPPnoaa20chlaDaily
    if(file.exists(paste(tmpdir,"/nesdisVHNnoaaSNPPnoaa20chlaDaily.nc",sep=""))==TRUE){
      print("Preparing nesdisVHNnoaaSNPPnoaa20chlaDaily")
      chla=raster(paste(tmpdir,"/nesdisVHNnoaaSNPPnoaa20chlaDaily.nc",sep=""),varname="chl_oci")
      print("Preparing nesdisVHNnoaaSNPPnoaa20chlaDaily: regridding to .25 x .25 degree resolution and taking the log")
      r=log(raster::resample(chla, template, method="bilinear")+0.001)
      print(paste("nesdisVHNnoaaSNPPnoaa20chlaDaily regridding complete, placing final file in",finaldir,sep=""))
      writeRaster(r,paste(finaldir,"/l.blendChl",sep=""),overwite=T)
    }
    
    # Scenario 3: new layer for erdVHNchla1day
    if(file.exists(paste(tmpdir,"/erdVHNchla1day.nc",sep=""))==TRUE){
      print("Preparing erdVHNchla1day")
      chla=raster(paste(tmpdir,"/erdVHNchla1day.nc",sep=""),varname="chla")
      print("Preparing erdVHNchla1day: regridding to .25 x .25 degree resolution and taking the log")
      r=log(raster::resample(chla, template, method="bilinear")+0.001)
      print(paste("erdVHNchla1day regridding complete, placing final file in",finaldir,sep=""))
      writeRaster(r,paste(finaldir,"/l.blendChl",sep=""),overwite=T)
    }
    
  }
  
  print("**************************************************************************************")
  #static variables
  
  if(!file.exists(paste0(finaldir,"/z.grd"))){
    print(paste("Copying bathymetry files into folder for ",get_date,sep=""))
    files=as.list(c("z.grd","zsd.grd","z_pt25.grd","z.gri","zsd.gri","z_pt25.gri"))
    lapply(files,function(x) file.copy(paste(staticdir,x,sep=""),finaldir))
  }
  
  if(!file.exists(paste0(finaldir,"/lunillum.grd"))){
    print(paste("Calculating lunillum for ",get_date,sep=""))
    value <- lunar.illumination(get_date)
    lunar_ras=template
    values(lunar_ras)=value
    writeRaster(lunar_ras,paste(finaldir,"/lunillum",sep=""),overwrite=T)
  }
  
}

}

Get_Env_Data_B_1(path = path,source_path = source_path)

